module type S = sig
(** Signature which all containers must satisfy *)

  type t
    (** Type for stores *)

  type repo
    (** Type for repositories *)

  type branch
    (** Type for persistent branch identifiers *)

  type key
    (** Type for keys *) 

  type contents
    (** Type for contents *)

  module Store : Irmin.S
    (** Store is a contents store *)

  val init : ?bare:bool -> root:string -> unit -> repo Lwt.t
    (**Initialises the store *)

  val master : repo -> t Lwt.t
    (** Fetches the master branch *)

  val of_branch : repo -> branch -> t Lwt.t
    (** Creates a new branch *)

  val clone : src:t -> dst:branch -> t Lwt.t
    (** Clones the repository *)

  val merge_into : t -> into:t -> unit Lwt.t
    (** Merges one branch into another *)

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


