open Lwt.Infix

let return = Lwt.return
let info = Irmin_unix.info

module Counter : Irmin.Contents.S with type t = int64 = struct
  type t = int64
  
  let t = Irmin.Type.int64
  
  let merge = Irmin.Merge.(option counter)
end

module type S = sig
  include Containers.S

  type value

  val inc : ?by:int64 -> t -> path:key -> unit Lwt.t
  val dec : ?by:int64 -> t -> path:key -> unit Lwt.t
  val set : ?v:int64 -> t -> path:key -> unit Lwt.t
  val read : t -> path:key -> value Lwt.t
end

module Make (Backend : Irmin.S_MAKER) (M : Irmin.Metadata.S) (P : Irmin.Path.S) (B : Irmin.Branch.S) (H : Irmin.Hash.S) : sig 
  include S with type value = int64
             and type key = P.t
             and type branch = B.t
end = struct
  module Repo = Containers.Make(Backend)(M)(Counter)(P)(B)(H)
  include Repo

  type value = Counter.t

  let modify by t path fn = 
    Store.find t path >>= function
      | None -> Store.set_exn ~info:(info "init set") t path (fn 0L by)
      | Some v -> Store.set_exn ~info:(info "update") t path (fn v by)

  let inc ?(by = 1L) t ~path = modify by t path (fun x by -> Int64.add x by)

  let dec ?(by = 1L) t ~path = modify by t path (fun x by -> Int64.sub x by)

  let set ?(v = 0L) t ~path = Store.set_exn ~info:(info "set") t path v

  let read t ~path =
    Store.find t path >>= function
      | None -> return 0L
      | Some v -> return v
end

 module Quick : sig
   include S with type value = int64
              and type key = string list
              and type branch = string
 end = Make(Irmin_unix.FS.Make)(Irmin.Metadata.None)(Irmin.Path.String_list)(Irmin.Branch.String)(Irmin.Hash.SHA1)

