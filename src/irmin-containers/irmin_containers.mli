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

(** Irmin_containers public API

    [Irmin_containers] is a collection of simple, ready-to-use mergeable data
    structures. Each data structure is customisable according to the user's
    needs with regards to the type of backend being used and its accompanying
    parameters. Along with that, [Irmin_containers] also provides an option to
    quickly use the data structure in which suitable instantiations have been
    performed. The quick use data structures are initialised for two backends:
    the {{!Irmin_mem} in-memory backend} provided by [Irmin_mem] and
    {{!Irmin_unix.FS} FS backend} provided by [Irmin_unix]. *)

(** {1 Data structures}*)

(** {2 Counter}

    [Counter] is the irmin implementation of the counter data structure. This
    module supports an int64 counter along with operations to increment,
    decrement and read the value of the counter. *)
module Counter : sig
  (** Counter signature *)
  module type S = sig
    include Counter.S
    (** @inline *)
  end

  (** Constructor for counter *)
  module Make
      (Backend : Irmin.S_MAKER)
      (M : Irmin.Metadata.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) :
    S
      with type value = int64
       and type Store.key = P.t
       and type Store.branch = B.t

  (** Ready-to-use counter implementations *)
  module Quick : sig
    (** Uses {{!Irmin_unix.FS} FS backend} provided by [Irmin_unix] *)
    module FS :
      S
        with type value = int64
         and type Store.key = string list
         and type Store.branch = string

    (** Uses {{!Irmin_mem} in-memory backend} provided by [Irmin_mem] *)
    module Mem :
      S
        with type value = int64
         and type Store.key = string list
         and type Store.branch = string
  end
end

(** {2 Last-write-wins register}

    [Lww_register] is the implementation of the last-write-wins register. The
    type of value to be stored in the register as well as the method to obtain
    timestamps are provided by the user. This module supports reading and
    writing to the register. *)
module Lww_register : sig
  (** [Input] specifies the type of value to be stored in the register. *)
  module type Input = sig
    include Lww_register.Input
    (** @inline *)
  end

  (** [Time] specifies the method to obtain timestamps for the values to be
      stored. It is necessary for the timestamps to be monotonic for the
      register to function properly. *)
  module type Time = sig
    include Lww_register.Time
    (** @inline *)
  end

  (** Lww_register signature *)
  module type S = sig
    include Lww_register.S
    (** @inline *)
  end

  (** Constructor for last-write-wins register *)
  module Make
      (Backend : Irmin.S_MAKER)
      (M : Irmin.Metadata.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S)
      (T : Time)
      (V : Input) :
    S with type value = V.t and type Store.key = P.t and type Store.branch = B.t

  (** Ready-to-use last-write-wins register implementations. Input must be
      provided to specify the type of value being stored. The timestamp is
      obtained using [Unix.gettimeofday ()] *)
  module Quick : sig
    (** Uses {{!Irmin_unix.FS} FS backend} provided by [Irmin_unix] *)
    module FS (V : Input) :
      S
        with type value = V.t
         and type Store.key = string list
         and type Store.branch = string

    (** Uses {{!Irmin_mem} in-memory backend} provided by [Irmin_mem] *)
    module Mem (V : Input) :
      S
        with type value = V.t
         and type Store.key = string list
         and type Store.branch = string
  end
end
