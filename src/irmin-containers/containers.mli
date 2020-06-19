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
 
module type S = sig
(** Signature which all containers must satisfy *)

  type t
    (** Type for stores *)

  type repo
    (** Type for repositories *)

  type branch
    (** Type for persistent branch identifiers *)

  type key
    (** Type for keys *) 

  type contents
    (** Type for contents *)

  module Store : Irmin.S
    (** Store is a contents store *)

  val init : ?bare:bool -> root:string -> unit -> repo Lwt.t
    (**Initialises the store *)

  val master : repo -> t Lwt.t
    (** Fetches the master branch *)

  val of_branch : repo -> branch -> t Lwt.t
    (** Creates a new branch *)

  val clone : src:t -> dst:branch -> t Lwt.t
    (** Clones the repository *)

  val merge_into : t -> into:t -> unit Lwt.t
    (** Merges one branch into another *)

end

module Make (Backend : Irmin.S_MAKER) 
            (M : Irmin.Metadata.S) 
            (C : Irmin.Contents.S) 
            (P : Irmin.Path.S) 
            (B : Irmin.Branch.S) 
            (H : Irmin.Hash.S) 
  :
    S with type t = Backend(M)(C)(P)(B)(H).t
       and type repo = Backend(M)(C)(P)(B)(H).repo
       and type branch = B.t 
       and type key = P.t
       and type contents = C.t
       and module Store = Backend(M)(C)(P)(B)(H)


