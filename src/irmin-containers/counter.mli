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
  (** Signature of counter *)

  include Containers.S

  type value
  (** Type of value as seen by the user *)

  val inc : ?by:int64 -> t -> path:key -> unit Lwt.t
  (** Increment the counter by the given amount. Default 1L *)

  val dec : ?by:int64 -> t -> path:key -> unit Lwt.t
  (** Decrement the counter by the given amount. Default 1L *)

  val set : ?v:int64 -> t -> path:key -> unit Lwt.t
  (** Set the counter to the given value. Default 0L *)

  val read : t -> path:key -> value Lwt.t
  (** Read the value of the counter *)
end

module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) :
  S with type value = int64 and type key = P.t and type branch = B.t

(** With suitable instantiations to quickly use counter *)
module Quick :
  S with type value = int64 and type branch = string and type key = string list
