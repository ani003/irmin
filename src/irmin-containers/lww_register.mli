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

(** Lww_register is the implementation of last-write-wins register. The value to
    be stored in the register and the timestamp method are provided by the user.
    Merge semantics : The value with the largest timestamp is chosen. If two
    values have the same timestamp, then the larger value is selected based on
    the compare specified by the user. *)

(** Input specifies the type of value to be stored in the register. *)
module type Input = sig
  include Irmin.Type.S
  (** Type information about the value to be stored. *)

  val compare : t -> t -> int
  (** Comparator for values of the type specified above. This is used to break
      ties while merging *)
end

(** Time specifies the method to obtain timestamps for the values to be stored.
    It is necessary for the timestamps to be monotonic for the register to
    function properly. *)
module type Time = sig
  type t
  (** Type of the timestamp *)

  val t : t Irmin.Type.t
  (** Corresponding irmin type of the timestamp *)

  val compare : t -> t -> int
  (** Comparator for the timestamps. Used to decide the last entry to the
      register.*)

  val get_time : unit -> t
  (** Returns a timestamp *)
end

(** Signature of the last-write-wins register. *)
module type S = sig
  include Containers.S
  (** General store related functions. *)

  type value
  (** Type of value stored in the register *)

  val read : Store.t -> path:Store.key -> value option Lwt.t
  (** Reads the value from the register. Returns None if no value is written *)

  val write :
    info:Irmin.Info.f -> Store.t -> path:Store.key -> value -> unit Lwt.t
  (** Writes the provided value to the register *)
end

(** Make returns a mergeable last-write-wins register using the backend and
    other parameters as specified by the user. *)
module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (T : Time)
    (V : Input) :
  S with type value = V.t and type Store.key = P.t and type Store.branch = B.t

(** Quick is the ready-to-use mergeable last-write-wins register. Input must be
    provided by the user to specify the type of value being stored. The
    timestamp is obtained using Unix.gettimeofday *)
module Quick : sig
  (** Uses the FS backend provided by Irmin_unix *)
  module FS (V : Input) :
    S
      with type value = V.t
       and type Store.key = string list
       and type Store.branch = string

  (** Uses the in-memory backend provided by Irmin_mem *)
  module Mem (V : Input) :
    S
      with type value = V.t
       and type Store.key = string list
       and type Store.branch = string
end
