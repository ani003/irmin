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

let empty_info = Irmin.Info.none

module Log_item (T : Time.S) (K : Irmin.Hash.S) (V : Irmin.Type.S) = struct
  type t = { time : T.t; msg : V.t; prev : K.t option }

  let t =
    let open Irmin.Type in
    record "t" (fun time msg prev -> { time; msg; prev })
    |+ field "time" T.t (fun r -> r.time)
    |+ field "msg" V.t (fun r -> r.msg)
    |+ field "prev" (option K.t) (fun r -> r.prev)
    |> sealr
end

module Store_item (T : Time.S) (K : Irmin.Hash.S) (V : Irmin.Type.S) = struct
  module L = Log_item (T) (K) (V)

  type t = Value of L.t | Merge of L.t list

  let t =
    let open Irmin.Type in
    variant "t" (fun value merge ->
      function Value v -> value v | Merge l -> merge l)
    |~ case1 "Value" L.t (fun v -> Value v)
    |~ case1 "Merge" (list L.t) (fun l -> Merge l)
    |> sealv
end

module Linked_log
    (C : Stores.Cas_maker)
    (T : Time.S)
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S) =
struct
  type t = K.t

  let t = K.t

  module L = Log_item (T) (K) (V)
  module S = Store_item (T) (K) (V)

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
    | None -> failwith "key not found in the store"
    | Some v -> return v

  let sort l =
    let compare = Irmin.Type.compare T.t in
    List.sort (fun i1 i2 -> compare i2.L.time i1.L.time) l

  let merge ~old:_ v1 v2 =
    let open Irmin.Merge in
    Store.get_store () >>= fun store ->
    Store.find store v1 >>= fun v1 ->
    Store.find store v2 >>= fun v2 ->
    let convert_to_list = function
      | None -> []
      | Some (S.Value v) -> [ v ]
      | Some (S.Merge lv) -> lv
    in
    let lv1 = convert_to_list v1 in
    let lv2 = convert_to_list v2 in
    Store.batch store (fun t -> Store.add t (S.Merge (sort @@ lv1 @ lv2)))
    >>= ok

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  include Blob_log.S

  type cursor

  val get_cursor : path:Store.key -> Store.t -> cursor Lwt.t

  val read : num_items:int -> cursor -> (value list * cursor) Lwt.t
end

module Make
    (Backend : Stores.Store_maker)
    (C : Stores.Cas_maker)
    (T : Time.S)
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S)
    () =
struct
  module L = Linked_log (C) (T) (K) (V)
  module Store = Backend (L)

  module Set_elt = struct
    type t = K.t

    let compare = Irmin.Type.compare K.t
  end

  module HashSet = Set.Make (Set_elt)

  type value = V.t

  type cursor = {
    seen : HashSet.t;
    cache : Log_item(T)(K)(V).t list;
    store : Store.t;
  }

  let append ~path t e =
    Store.find t path >>= fun prev ->
    L.append prev e >>= fun v -> Store.set_exn ~info:empty_info t path v

  let get_cursor ~path store =
    let mk_cursor seen cache = { seen; cache; store } in
    Store.find store path >>= function
    | None -> return (mk_cursor HashSet.empty [])
    | Some k -> (
        L.read_key k >|= function
        | Value v -> mk_cursor (HashSet.singleton k) [ v ]
        | Merge l -> mk_cursor (HashSet.singleton k) l )

  let rec read_log cursor num_items acc =
    if num_items <= 0 then return (List.rev acc, cursor)
    else
      match cursor.cache with
      | [] -> return (List.rev acc, cursor)
      | { msg; prev = None; _ } :: xs ->
          read_log { cursor with cache = xs } (num_items - 1) (msg :: acc)
      | { msg; prev = Some pk; _ } :: xs -> (
          if HashSet.mem pk cursor.seen then
            read_log { cursor with cache = xs } (num_items - 1) (msg :: acc)
          else
            let seen = HashSet.add pk cursor.seen in
            L.read_key pk >>= function
            | Value v ->
                read_log
                  { cursor with seen; cache = L.sort (v :: xs) }
                  (num_items - 1) (msg :: acc)
            | Merge l ->
                read_log
                  { cursor with seen; cache = L.sort (l @ xs) }
                  (num_items - 1) (msg :: acc) )

  let read ~num_items cursor = read_log cursor num_items []

  let read_all ~path t = get_cursor t ~path >>= read ~num_items:max_int >|= fst
end

module FS (C : Stores.Cas_maker) (V : Irmin.Type.S) () =
  Make (Irmin_unix.FS.KV) (C) (Time.Unix) (Irmin.Hash.SHA1) (V) ()

module Mem (C : Stores.Cas_maker) (V : Irmin.Type.S) () =
  Make (Irmin_mem.KV) (C) (Time.Unix) (Irmin.Hash.SHA1) (V) ()
