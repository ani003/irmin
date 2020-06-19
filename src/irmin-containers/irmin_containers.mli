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

(** Irmin_containers public API *)

module Containers : sig
  (** API for containers *)

  module type S = sig
    include Containers.S
  end

  module Make (Backend : Irmin.S_MAKER) 
              (M : Irmin.Metadata.S) 
              (C : Irmin.Contents.S)             
              (P : Irmin.Path.S) 
              (B : Irmin.Branch.S) 
              (H : Irmin.Hash.S) 
            
    :
      S with type t = Backend(M)(C)(P)(B)(H).t
         and type repo = Backend(M)(C)(P)(B)(H).repo
         and type branch = B.t 
         and type key = P.t
         and type contents = C.t
         and module Store = Backend(M)(C)(P)(B)(H)

end

module Counter : sig
  (** API for counter *)

  module type S = sig
    include Counter.S
  end

  module Make (Backend : Irmin.S_MAKER)
              (M : Irmin.Metadata.S)
              (P : Irmin.Path.S) 
              (B : Irmin.Branch.S) 
              (H : Irmin.Hash.S)
    :  
      S with type value = int64
         and type key = P.t
         and type branch = B.t

  module Quick : S with type value = int64
                    and type branch = string
                    and type key = string list

end

module Lww_register : sig
  (** API for LWW register *)

  module type Input = sig 
    include Lww_register.Input
  end

  module type S = sig
    include Lww_register.S
  end

  module Make (Backend : Irmin.S_MAKER)
              (M : Irmin.Metadata.S)
              (P : Irmin.Path.S)
              (B : Irmin.Branch.S)
              (H : Irmin.Hash.S)
              (V : Input)
    :
      S with type value = V.t
         and type key = P.t 
         and type branch = B.t

  module Quick (V : Input) : S with type value = V.t
                                and type key = string list
                                and type branch = string
end


