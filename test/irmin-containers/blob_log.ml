open Lwt.Infix

module String = struct
  type t = string

  let t = Irmin.Type.string
end

module M = Irmin_containers.Blob_log.Mem (String)
open M

let return = Lwt.return

let info = Irmin_unix.info

let conf () = Store.Repo.v (Irmin_mem.config ())

let path = [ "tmp"; "blob" ]

let test_append_read () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  read_all t ~path >>= fun l ->
  assert (l = []);
  append ~info:(info "append to master") t ~path "master.1" >>= fun () ->
  append ~info:(info "append to master") t ~path "master.2" >>= fun () ->
  read_all t ~path >>= fun l ->
  assert (l = [ "master.2"; "master.1" ]);
  return ()

let test_branch_append_read () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  Store.clone ~src:t ~dst:"br1" >>= fun b ->
  append ~info:(info "append to clone") b ~path "clone.1" >>= fun () ->
  append ~info:(info "append to master") t ~path "master.3" >>= fun () ->
  append ~info:(info "append to clone") b ~path "clone.2" >>= fun () ->
  (Store.merge_into ~info:(info "merging clone into master") b ~into:t
   >>= function
   | Error _ -> failwith "error"
   | Ok () -> return ())
  >>= fun () ->
  append ~info:(info "append to master") t ~path "master.4" >>= fun () ->
  read_all t ~path >>= fun l ->
  assert (
    l = [ "master.4"; "clone.2"; "master.3"; "clone.1"; "master.2"; "master.1" ]
  );
  return ()

let test_get_branch () =
  conf () >>= fun r ->
  Store.of_branch r "br2" >>= fun b ->
  append ~info:(info "append to branch") b ~path "branch.1" >>= fun () ->
  append ~info:(info "append to branch") b ~path "branch.2" >>= fun () ->
  read_all b ~path >>= fun l ->
  assert (l = [ "branch.2"; "branch.1" ]);
  return ()

let test () =
  test_append_read () >>= test_branch_append_read >>= test_get_branch
