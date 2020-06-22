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

let info = Irmin_unix.info

module Counter : Irmin.Contents.S with type t = int64 = struct
  type t = int64

  let t = Irmin.Type.int64

  let merge = Irmin.Merge.(option counter)
end

module type S = sig
  include Containers.S

  type value

  val inc : ?by:value -> Store.t -> path:Store.key -> unit Lwt.t

  val dec : ?by:value -> Store.t -> path:Store.key -> unit Lwt.t

  val read : Store.t -> path:Store.key -> value Lwt.t
end

module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) : sig
  include
    S
      with type value = int64
       and type Store.key = P.t
       and type Store.branch = B.t
end = struct
  module Repo = Containers.Make (Backend) (M) (Counter) (P) (B) (H)
  include Repo

  type value = Counter.t

  let modify by t path fn =
    Store.find t path >>= function
    | None -> Store.set_exn ~info:(info "init set") t path (fn 0L by)
    | Some v -> Store.set_exn ~info:(info "update") t path (fn v by)

  let inc ?(by = 1L) t ~path = modify by t path (fun x by -> Int64.add x by)

  let dec ?(by = 1L) t ~path = modify by t path (fun x by -> Int64.sub x by)

  let read t ~path =
    Store.find t path >>= function None -> return 0L | Some v -> return v
end

module Quick : sig
  include
    S
      with type value = int64
       and type Store.key = string list
       and type Store.branch = string
end =
  Make (Irmin_unix.FS.Make) (Irmin.Metadata.None) (Irmin.Path.String_list)
    (Irmin.Branch.String)
    (Irmin.Hash.SHA1)
