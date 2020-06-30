open Lwt.Infix
module C = Irmin_containers.Counter.Quick.Mem
open C

let return = Lwt.return

let info = Irmin_unix.info

let conf = init (Irmin_mem.config ())

let path = [ "tmp"; "cnt" ]

let test_inc_dec () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  inc ~info:(info "inc by 1") t ~path >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = 1L);
  inc ~info:(info "inc by 2") t ~path ~by:2L >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = 3L);
  dec ~info:(info "dec by 1") t ~path >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = 2L);
  dec ~info:(info "dec by 2") t ~path ~by:2L >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = 0L);
  return ()

let test_clone_merge () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  inc ~info:(info "inc master by 5") t ~path ~by:5L >>= fun () ->
  Store.clone ~src:t ~dst:"br1" >>= fun b ->
  inc ~info:(info "inc clone by 2") b ~path ~by:2L >>= fun () ->
  dec ~info:(info "dec master by 4") t ~path ~by:4L >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = 1L);
  read b ~path >>= fun x ->
  assert (x = 7L);
  (Store.merge_into ~info:(info "merging clone into master") b ~into:t
   >>= function
   | Error _ -> failwith "error"
   | Ok _ -> read t ~path)
  >>= fun x ->
  assert (x = 3L);
  return ()

let test_branch_merge () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  inc ~info:(info "inc master by 7") t ~path ~by:7L >>= fun () ->
  Store.of_branch r "br2" >>= fun b ->
  inc ~info:(info "inc master by 1") t ~path >>= fun () ->
  dec ~info:(info "dec branch by 11") b ~path ~by:11L >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = 11L);
  read b ~path >>= fun x ->
  assert (x = -11L);
  (Store.merge_into ~info:(info "merging branch into master") b ~into:t
   >>= function
   | Error _ -> failwith "error"
   | Ok _ -> read t ~path)
  >>= fun x ->
  assert (x = 0L);
  return ()

let test () = test_inc_dec () >>= test_clone_merge >>= test_branch_merge
