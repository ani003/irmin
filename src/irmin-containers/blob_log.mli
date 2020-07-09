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

(** [Blob_log] is the implementation of the log in which two copies of the log
    do not share the common predecessor. The type of the log entries and the
    method to obtain timestamp are provided by the user. Merging does the
    following: the newer entries from each branch, with respect to the least
    common ancestor, are taken, merged and then appended in front of the LCA *)

(** Signature of [Blob_log] *)
module type S = sig
  module Store : Irmin.S
  (** Store for the blob log. All store related operations like branching,
      cloning, merging, etc are done through this module. *)

  type value
  (** Type of log entry *)

  val append : path:Store.key -> Store.t -> value -> unit Lwt.t
  (** Append an entry to the log *)

  val read_all : path:Store.key -> Store.t -> value list Lwt.t
  (** Read the entire log *)
end

(** [Make] returns a mergeable blob log using the backend and other parameters
    as specified by the user. *)
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
