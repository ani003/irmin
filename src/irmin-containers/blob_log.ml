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

module Blob_log (T : Time.S) (V : Irmin.Type.S) :
  Irmin.Contents.S with type t = (V.t * T.t) list = struct
  type t = (V.t * T.t) list

  let t = Irmin.Type.(list (pair V.t T.t))

  let compare (_, t1) (_, t2) = T.compare t1 t2

  let newer_than timestamp entries =
    let rec util acc = function
      | [] -> List.rev acc
      | (_, x) :: _ when T.compare x timestamp <= 0 -> List.rev acc
      | h :: t -> util (h :: acc) t
    in
    util [] entries

  let merge ~old v1 v2 =
    let open Irmin.Merge.Infix in
    let ok = Irmin.Merge.ok in
    old () >>=* fun old ->
    let old = match old with None -> [] | Some o -> o in
    let l1, l2 =
      match old with
      | [] -> (v1, v2)
      | (_, t) :: _ -> (newer_than t v1, newer_than t v2)
    in
    let l3 = List.sort compare (List.rev_append l1 l2) in
    ok (List.rev_append l3 old)

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  module Store : Irmin.S

  type value

  val append :
    info:Irmin.Info.f -> Store.t -> path:Store.key -> value -> unit Lwt.t

  val read_all : Store.t -> path:Store.key -> value list Lwt.t
end

module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (T : Time.S)
    (V : Irmin.Type.S) : sig
  include
    S with type value = V.t and type Store.key = P.t and type Store.branch = B.t
end = struct
  module Store = Backend (M) (Blob_log (T) (V)) (P) (B) (H)

  type value = V.t

  let create_entry v = (v, T.get_time ())

  let append ~info t ~path v =
    Store.find t path >>= function
    | None -> Store.set_exn ~info t path [ create_entry v ]
    | Some l -> Store.set_exn ~info t path (create_entry v :: l)

  let read_all t ~path =
    Store.find t path >>= function
    | None -> return []
    | Some l -> return (List.map (fun (v, _) -> v) l)
end

module FS (V : Irmin.Type.S) =
  Make (Irmin_unix.FS.Make) (Irmin.Metadata.None) (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
    (Time.Unix)
    (V)
module Mem (V : Irmin.Type.S) =
  Make (Irmin_mem.Make) (Irmin.Metadata.None) (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
    (Time.Unix)
    (V)
