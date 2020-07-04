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
    parameters. Along with that, [Irmin_containers] also provides data
    structures with suitable instantiations performed using two backends: the
    {{!Irmin_mem} in-memory backend} provided by [Irmin_mem] and
    {{!Irmin_unix.FS} FS backend} provided by [Irmin_unix]. *)

(** {1 Auxiliary modules} *)

(** {2 Time}

    [Time] specifies the method to obtain timestamps for the values to be
    stored. It is necessary for the timestamps to be monotonic for the data
    structures to function properly. *)
module Time : sig
  (** Signature for [Time] *)
  module type S = sig
    include Time.S
    (** @inline *)
  end

  module Unix : S
  (** A timestamp method using [Unix.gettimeofday] *)
end

(** {2 Cas_maker}

    [Cas_maker] is the content addressable store maker required by some of the
    data structures. *)
module Cas_maker : sig
  (** [Cas_maker] Signature *)
  module type S = sig
    include Cas_maker.S
    (** @inline *)
  end

  module Mem : S
  (** A CAS maker implementation using {!Irmin_mem} *)
end

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

  (** Counter instantiated using {{!Irmin_unix.FS} FS backend} provided by
      [Irmin_unix] *)
  module FS :
    S
      with type value = int64
       and type Store.key = string list
       and type Store.branch = string

  (** Counter instantiated using {{!Irmin_mem} in-memory backend} provided by
      [Irmin_mem] *)
  module Mem :
    S
      with type value = int64
       and type Store.key = string list
       and type Store.branch = string
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

  (** LWW register instantiated using {{!Irmin_mem} in-memory backend} provided
      by [Irmin_mem] and the timestamp method {!Time.Unix} *)
  module Mem (V : Input) :
    S
      with type value = V.t
       and type Store.key = string list
       and type Store.branch = string
end

(** {2 Blob log}

    [Blob_log] is the implementation of log in which two copies do not share
    their common predecessor. Each copy is maintained as a separate log. The
    blob log supports appending an entry into the log and reading the entire
    log. *)
module Blob_log : sig
  (** Signature of the blob log *)
  module type S = sig
    include Blob_log.S
    (** @inline *)
  end

  (** Constructor for blob log *)
  module Make
      (Backend : Irmin.S_MAKER)
      (M : Irmin.Metadata.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S)
      (T : Time.S)
      (V : Irmin.Type.S) :
    S with type value = V.t and type Store.key = P.t and type Store.branch = B.t

  (** Blob log instantiated using {{!Irmin_unix.FS} FS backend} provided by
      [Irmin_unix] and the timestamp method {!Time.Unix} *)
  module FS (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.key = string list
       and type Store.branch = string

  (** Blob log instantiated using {{!Irmin_mem} in-memory backend} provided by
      [Irmin_mem] and the timestamp method {!Time.Unix} *)
  module Mem (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.key = string list
       and type Store.branch = string
end
