module type Input = sig
  (** Signature for the input argument *)

  include Irmin.Type.S
  
  val compare : t -> t -> int
    (** Comparator for values of type t *)

end

module type S = sig
  (** Signature of the LWW register *)

  include Containers.S

  type value
    (** Type of value stored in the register *)
  
  val read : t -> path:key -> value option Lwt.t
    (** Reads the value from the register. Returns None if no value is written *)
  
  val write : t -> path:key -> value -> unit Lwt.t
    (** Writes value to the LWW register *)

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
  (** With suitable instantiations to quickly use LWW register *)

