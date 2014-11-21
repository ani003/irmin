(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Merge = Ir_merge
module Origin = Ir_origin
module Contents = Ir_contents
module Store = struct
  module type RO = Ir_ro.STORE
  module type AO = Ir_ao.STORE
  module type RW = Ir_rw.STORE
  module type BC = Ir_bc.STORE
end
module View = Ir_view
module Hash = Ir_hash

module type S = sig
  type step
  include Store.BC with type key = step list
  module View: Ir_view.OF_STORE
    with type db = t
     and type origin = origin
     and type step = step
     and type value = value
  module Snapshot: Ir_snapshot.OF_STORE
    with type db = t
     and type origin = origin
     and type key = key
     and type value = value
  module Dump: Ir_dump.OF_STORE
    with type db = t
     and type origin = origin
end
