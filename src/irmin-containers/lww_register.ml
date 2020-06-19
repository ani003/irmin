open Lwt.Infix

let return = Lwt.return
let info = Irmin_unix.info

module type Input = sig
  include Irmin.Type.S

  val compare : t -> t -> int
end

module Time : sig
  type t

  val t : t Irmin.Type.t
  val compare : t -> t -> int
  val get_time : unit -> t

end = struct
  type t = int64

  let t = Irmin.Type.int64

  let compare = Int64.compare

  let get_time () = Unix.gettimeofday () |> Int64.of_float
end


module LWW (V : Input) : Irmin.Contents.S with type t = V.t * Time.t = struct
  type t = V.t * Time.t
           
  let t = Irmin.Type.(pair V.t Time.t)

  let compare (v1, t1) (v2, t2) = 
    let res = Time.compare t1 t2 in
    if res = 0 then V.compare v1 v2 else res

  let merge ~old v1 v2 = 
    let open Irmin.Merge in
    if compare v1 v2 > 0 then ok v1 else ok v2

  let merge = Irmin.Merge.(option (v t merge))
end

module type S = sig
  include Containers.S

  type value

  val read : t -> path:key -> value option Lwt.t
  val write : t -> path:key -> value -> unit Lwt.t
end

module Make (Backend : Irmin.S_MAKER) (M : Irmin.Metadata.S) (P : Irmin.Path.S) (B : Irmin.Branch.S) (H : Irmin.Hash.S) (V : Input): sig
  include S with type value = V.t
             and type key = P.t
             and type branch = B.t 
end = struct
  module Repo = Containers.Make(Backend)(M)(LWW(V))(P)(B)(H)
  include Repo

  type value = V.t

  let read t ~path = 
    Store.find t path >>= function
    | None -> return None
    | Some (v, _) -> return (Some v)

  let write t ~path v = 
    let timestamp = Time.get_time() in
    Store.set_exn ~info:(info "writing") t path (v, timestamp)
end

 module Quick (V : Input) : sig
    include S with type value = V.t
               and type key = string list
               and type branch = string
  end = Make(Irmin_unix.FS.Make)(Irmin.Metadata.None)(Irmin.Path.String_list)(Irmin.Branch.String)(Irmin.Hash.SHA1)(V)

