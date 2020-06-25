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

    Irmin_containers is a collection of simple, ready-to-use mergeable data
    structures. Each data structure is customisable according to the user's
    needs with regards to the type of backend being used and its accompanying
    parameters. Along with that, Irmin_containers also provides an option to
    quickly use the data structure in which suitable instantiations have been
    performed. The quick use data structures use the FS backend provided by
    Irmin_unix. *)

(** Containers module provides the general signature upon which all data
    structures are are created. All data structures share the Containers.S
    signature and data structure specific functions are added on top of that. *)
module Containers : sig
  module type S = sig
    include Containers.S
  end

  module Make
      (Backend : Irmin.S_MAKER)
      (M : Irmin.Metadata.S)
      (C : Irmin.Contents.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) : S with module Store = Backend(M)(C)(P)(B)(H)
end

(** Counter is the irmin implementation of the counter data structure. This
    module supports an int64 counter along with operations to increment,
    decrement and read the value of the counter. *)
module Counter : sig
  module type S = sig
    include Counter.S
  end

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

  module Quick :
    S
      with type value = int64
       and type Store.key = string list
       and type Store.branch = string
end

(** Lww_register is the implementation of the last-write-wins register. The type
    of value to be stored in the register as well as the method to obtain
    timestamps are provided ny the user. This module supports reading and
    writing to the register. *)
module Lww_register : sig
  module type Input = sig
    include Lww_register.Input
  end

  module type Time = sig
    include Lww_register.Time
  end

  module type S = sig
    include Lww_register.S
  end

  module Make
      (Backend : Irmin.S_MAKER)
      (M : Irmin.Metadata.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S)
      (T : Time)
      (V : Input) :
    S with type value = V.t and type Store.key = P.t and type Store.branch = B.t

  module Quick (V : Input) :
    S
      with type value = V.t
       and type Store.key = string list
       and type Store.branch = string
end
