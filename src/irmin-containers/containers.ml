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

module type S = sig
  type t
  type repo
  type branch
  type key
  type contents

  module Store : Irmin.S

  val init : ?bare:bool -> root:string -> unit -> repo Lwt.t
  val master : repo -> t Lwt.t
  val of_branch : repo -> branch -> t Lwt.t
  val clone : src:t -> dst:branch -> t Lwt.t
  val merge_into : t -> into:t -> unit Lwt.t
  
end

module Make (Backend : Irmin.S_MAKER) (M : Irmin.Metadata.S) (C : Irmin.Contents.S) (P : Irmin.Path.S) (B : Irmin.Branch.S) (H : Irmin.Hash.S) : sig
  include S with type t = Backend(M)(C)(P)(B)(H).t
             and type repo = Backend(M)(C)(P)(B)(H).repo
             and type branch = B.t
             and type key = P.t
             and type contents = C.t 
             and module Store = Backend(M)(C)(P)(B)(H)
end = struct
  module Store = Backend(M)(C)(P)(B)(H)

  type t = Store.t
  type repo = Store.repo
  type branch = Store.branch
  type key = Store.key
  type contents = Store.contents

  let init ?bare ~root () =
    let config = Irmin_git.config ?bare root in
    Store.Repo.v config

  let master r = Store.master r

  let of_branch r b = Store.of_branch r b

  let clone ~src ~dst = Store.clone ~src ~dst

  let merge_into t ~into = 
    Store.merge_into ~info:(Irmin_unix.info "Merging") t ~into >>= function
    | Ok _ -> Lwt.return_unit
    | Error _ -> failwith "Merging conflict"
end
