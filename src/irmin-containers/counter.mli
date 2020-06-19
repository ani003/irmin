module type S = sig
  (** Signature of counter *)

  include Containers.S

  type value
    (** Type of value as seen by the user *)

  val inc : ?by:int64 -> t -> path:key -> unit Lwt.t
    (** Increment the counter by the given amount. Default 1L *)
  
  val dec : ?by:int64 -> t -> path:key -> unit Lwt.t
    (** Decrement the counter by the given amount. Default 1L *)
  
  val set : ?v:int64 -> t -> path:key -> unit Lwt.t
    (** Set the counter to the given value. Default 0L *)
  
  val read : t -> path:key -> value Lwt.t
    (** Read the value of the counter *)

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
  (** With suitable instantiations to quickly use counter *)


