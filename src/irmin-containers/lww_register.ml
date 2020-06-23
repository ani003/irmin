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

module type Input = sig
  include Irmin.Type.S

  val compare : t -> t -> int
end

module type Time = sig
  type t

  val t : t Irmin.Type.t

  val compare : t -> t -> int

  val get_time : unit -> t
end

module LWW (T : Time) (V : Input) : Irmin.Contents.S with type t = V.t * T.t =
struct
  type t = V.t * T.t

  let t = Irmin.Type.(pair V.t T.t)

  let compare (v1, t1) (v2, t2) =
    let res = T.compare t1 t2 in
    if res = 0 then V.compare v1 v2 else res

  let merge ~old:_ v1 v2 =
    let open Irmin.Merge in
    if compare v1 v2 > 0 then ok v1 else ok v2

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  include Containers.S

  type value

  val read : Store.t -> path:Store.key -> value option Lwt.t

  val write :
    info:Irmin.Info.f -> Store.t -> path:Store.key -> value -> unit Lwt.t
end

module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (T : Time)
    (V : Input) : sig
  include
    S with type value = V.t and type Store.key = P.t and type Store.branch = B.t
end = struct
  module Repo = Containers.Make (Backend) (M) (LWW (T) (V)) (P) (B) (H)
  include Repo

  type value = V.t

  let read t ~path =
    Store.find t path >>= function
    | None -> return None
    | Some (v, _) -> return (Some v)

  let write ~info t ~path v =
    let timestamp = T.get_time () in
    Store.set_exn ~info t path (v, timestamp)
end

module QuickTime : Time = struct
  type t = int64

  let t = Irmin.Type.int64

  let compare = Int64.compare

  let get_time () = Unix.gettimeofday () |> Int64.of_float
end

module Quick (V : Input) : sig
  include
    S
      with type value = V.t
       and type Store.key = string list
       and type Store.branch = string
end =
  Make (Irmin_unix.FS.Make) (Irmin.Metadata.None) (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
    (QuickTime)
    (V)
