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

(** [Linked_log] is a linked list implementation of the log. In this, two copies
    of the log share their common predecessor. The type of the log entries, the
    method to obtain timestamp, a hashing method and a content addressable store
    maker are provided by the user. *)

(** Signature of [Linked_log] *)
module type S = sig
  module Store : Irmin.S
  (** Store for the log. All store related operations like branching, cloning,
      merging, etc are done through this module. *)

  type value
  (** Type of log entry *)

  type cursor
  (** Type of cursor. Cursor is like a marker from which a certain number of
      entries can be read *)

  val append :
    info:Irmin.Info.f -> Store.t -> path:Store.key -> value -> unit Lwt.t
  (** Append an entry to the log *)

  val get_cursor : Store.t -> path:Store.key -> cursor option Lwt.t
  (** Get the cursor *)

  val read : cursor -> num_items:int -> (value list * cursor option) Lwt.t
  (** Read a certain number of entries starting from the cursor. If the number
      specified is greater than the number of log entires from the cursor, the
      log is read till the end. If the input cursor has already reached the end,
      then an empty list is returned *)

  val read_all : Store.t -> path:Store.key -> value list Lwt.t
  (** Read the entire log *)
end

(** [Make] returns a mergeable linked log using the backend and other parameters
    as specified by the user. *)
module Make
    (Backend : Irmin.S_MAKER)
    (M : Irmin.Metadata.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (C : Cas_maker.S)
    (T : Time.S)
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S) :
  S with type value = V.t and type Store.key = P.t and type Store.branch = B.t

(** Linked log instantiated using {{!Irmin_unix.FS} FS backend} provided by
    [Irmin_unix], timestamp method {!Time.Unix}, hash {!Irmin.Hash.SHA1} and CAS
    maker {!Cas_maker.Mem} *)
module FS (V : Irmin.Type.S) :
  S
    with type value = V.t
     and type Store.key = string list
     and type Store.branch = string

(** Linked log instantiated using {{!Irmin_mem} in-memory backend} provided by
    [Irmin_mem], timestamp method {!Time.Unix}, hash {!Irmin.Hash.SHA1} and CAS
    maker {!Cas_maker.Mem} *)
module Mem (V : Irmin.Type.S) :
  S
    with type value = V.t
     and type Store.key = string list
     and type Store.branch = string
