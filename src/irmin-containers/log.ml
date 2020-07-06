(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt.Infix

let return = Lwt.return

type ('a, 'b, 'c) log_item = { time : 'a; msg : 'b; prev : 'c option }

module Log_item (T : Time.S) (K : Irmin.Hash.S) (V : Irmin.Type.S) :
  Irmin.Type.S with type t = (T.t, V.t, K.t) log_item = struct
  type t = (T.t, V.t, K.t) log_item

  let t =
    let open Irmin.Type in
    record "t" (fun time msg prev -> { time; msg; prev })
    |+ field "time" T.t (fun r -> r.time)
    |+ field "msg" V.t (fun r -> r.msg)
    |+ field "prev" (option K.t) (fun r -> r.prev)
    |> sealr
end

type ('a, 'b, 'c) store_item =
  | Value of ('a, 'b, 'c) log_item
  | Merge of ('a, 'b, 'c) log_item list

module Store_item (T : Time.S) (K : Irmin.Hash.S) (V : Irmin.Type.S) :
  Irmin.Type.S with type t = (T.t, V.t, K.t) store_item = struct
  module L = Log_item (T) (K) (V)

  type t = (T.t, V.t, K.t) store_item

  let t =
    let open Irmin.Type in
    variant "t" (fun value merge ->
      function Value v -> value v | Merge l -> merge l)
    |~ case1 "Value" L.t (fun v -> Value v)
    |~ case1 "Merge" (list L.t) (fun l -> Merge l)
    |> sealv
end

module Log_ops
    (C : Cas_maker.S)
    (T : Time.S)
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S) =
struct
  module Store = struct
    include C.CAS_Maker (K) (Store_item (T) (K) (V))

    let get_store =
      let st = v @@ C.config in
      fun () -> st
  end

  let append prev msg =
    Store.get_store () >>= fun store ->
    Store.batch store (fun t ->
        Store.add t (Value { time = T.get_time (); msg; prev }))

  let read_key k =
    Store.get_store () >>= fun store ->
    Store.find store k >>= function
    | None -> failwith "not found"
    | Some v -> return v

  let sort l = List.sort (fun i1 i2 -> T.compare i2.time i1.time) l
end

module Log (C : Cas_maker.S) (T : Time.S) (K : Irmin.Hash.S) (V : Irmin.Type.S) =
struct
  type t = K.t

  let t = K.t

  include Log_ops (C) (T) (K) (V)

  let merge ~old:_ v1 v2 =
    let open Irmin.Merge in
    Store.get_store () >>= fun store ->
    Store.find store v1 >>= fun v1 ->
    Store.find store v2 >>= fun v2 ->
    let lv1 =
      match v1 with
      | None -> []
      | Some (Value v) -> [ v ]
      | Some (Merge lv) -> lv
    in
    let lv2 =
      match v2 with
      | None -> []
      | Some (Value v) -> [ v ]
      | Some (Merge lv) -> lv
    in
    Store.batch store (fun t -> Store.add t (Merge (sort @@ lv1 @ lv2)))
    >>= fun m -> ok m

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  module Store : Irmin.S

  type value

  type cursor

  val append :
    info:Irmin.Info.f -> Store.t -> path:Store.key -> value -> unit Lwt.t

  val get_cursor : Store.t -> path:Store.key -> cursor option Lwt.t

  val read : cursor -> num_items:int -> (value list * cursor option) Lwt.t

  val read_all : Store.t -> path:Store.key -> value list Lwt.t
end

module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (C : Cas_maker.S)
    (T : Time.S)
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S) : sig
  include
    S with type value = V.t and type Store.key = P.t and type Store.branch = B.t
end = struct
  module L = Log (C) (T) (K) (V)
  module Store = Backend (M) (L) (P) (B) (H)

  module Set_elt = struct
    type t = K.t

    let compare h1 h2 = K.short_hash h1 - K.short_hash h2
  end

  module HashSet = Set.Make (Set_elt)

  type value = V.t

  type cursor = {
    seen : HashSet.t;
    cache : (T.t, V.t, K.t) log_item list;
    store : Store.t;
  }

  let append ~info t ~path e =
    Store.find t path >>= fun prev ->
    L.append prev e >>= fun v -> Store.set_exn ~info t path v

  let get_cursor store ~path =
    let mk_cursor k cache =
      return @@ Some { seen = HashSet.singleton k; cache; store }
    in
    Store.find store path >>= function
    | None -> return None
    | Some k -> (
        L.read_key k >>= function
        | Value v -> mk_cursor k [ v ]
        | Merge l -> mk_cursor k l )

  let rec read_log cursor num_items acc =
    let open L in
    if num_items <= 0 then return (List.rev acc, Some cursor)
    else
      match cursor.cache with
      | [] -> return (List.rev acc, None)
      | { msg; prev = None; _ } :: xs ->
          read_log { cursor with cache = xs } (num_items - 1) (msg :: acc)
      | { msg; prev = Some pk; _ } :: xs -> (
          if HashSet.mem pk cursor.seen then
            read_log { cursor with cache = xs } (num_items - 1) (msg :: acc)
          else
            let seen = HashSet.add pk cursor.seen in
            read_key pk >>= function
            | Value v ->
                read_log
                  { cursor with seen; cache = sort (v :: xs) }
                  (num_items - 1) (msg :: acc)
            | Merge l ->
                read_log
                  { cursor with seen; cache = sort (l @ xs) }
                  (num_items - 1) (msg :: acc) )

  let read cursor ~num_items = read_log cursor num_items []

  let read_all t ~path =
    get_cursor t ~path >>= function
    | None -> return []
    | Some cursor ->
        read cursor ~num_items:max_int >>= fun (log, _) -> return log
end

module FS (V : Irmin.Type.S) =
  Make (Irmin_unix.FS.Make) (Irmin.Metadata.None) (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
    (Cas_maker.Mem)
    (Time.Unix)
    (Irmin.Hash.SHA1)
    (V)
module Mem (V : Irmin.Type.S) =
  Make (Irmin_mem.Make) (Irmin.Metadata.None) (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
    (Cas_maker.Mem)
    (Time.Unix)
    (Irmin.Hash.SHA1)
    (V)
