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


