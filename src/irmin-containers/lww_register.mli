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

module type Input = sig
  (** Signature for the input argument *)

  include Irmin.Type.S

  val compare : t -> t -> int
  (** Comparator for values of type t *)
end

module type S = sig
  (** Signature of the LWW register *)

  include Containers.S

  type value
  (** Type of value stored in the register *)

  val read : Store.t -> path:Store.key -> value option Lwt.t
  (** Reads the value from the register. Returns None if no value is written *)

  val write : info:Irmin.Info.f -> Store.t -> path:Store.key -> value -> unit Lwt.t
  (** Writes value to the LWW register *)
end

module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (V : Input) :
  S with type value = V.t and type Store.key = P.t and type Store.branch = B.t

(** With suitable instantiations to quickly use LWW register *)
module Quick (V : Input) :
  S
    with type value = V.t
     and type Store.key = string list
     and type Store.branch = string
