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

(** [Lww_register] is the implementation of last-write-wins register. The value
    to be stored in the register and the timestamp method are provided by the
    user. Merge semantics : The value with the largest timestamp is chosen. If
    two values have the same timestamp, then the larger value is selected based
    on the compare specified by the user. *)

(** [Input] specifies the type of value to be stored in the register. *)
module type Input = sig
  include Irmin.Type.S
  (** Type information about the value to be stored. *)

  val compare : t -> t -> int
  (** Comparator for values of the type specified above. This is used to break
      ties while merging *)
end

(** Signature of [Lww_register] *)
module type S = sig
  module Store : Irmin.S
  (** Content store of the register. All store related operations like
      branching, cloning, merging, etc are done through this module. *)

  type value
  (** Type of value stored in the register *)

  val read : path:Store.key -> Store.t -> value option Lwt.t
  (** Reads the value from the register. Returns None if no value is written *)

  val write :
    info:Irmin.Info.f -> path:Store.key -> Store.t -> value -> unit Lwt.t
  (** Writes the provided value to the register *)
end

(** [Make] returns a mergeable last-write-wins register using the backend and
    other parameters as specified by the user. *)
module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (T : Time.S)
    (V : Input) :
  S with type value = V.t and type Store.key = P.t and type Store.branch = B.t

(** LWW register instantiated using {{!Irmin_unix.FS} FS backend} provided by
    [Irmin_unix] and the timestamp method {!Time.Unix} *)
module FS (V : Input) :
  S
    with type value = V.t
     and type Store.key = string list
     and type Store.branch = string

(** LWW register instantiated using {{!Irmin_mem} in-memory backend} provided by
    [Irmin_mem] and the timestamp method {!Time.Unix} *)
module Mem (V : Input) :
  S
    with type value = V.t
     and type Store.key = string list
     and type Store.branch = string
