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

open Lwt.Infix

module In : Irmin_containers.Lww_register.Input with type t = int = struct
  type t = int

  let t = Irmin.Type.int

  let compare = Int.compare
end

module L = Irmin_containers.Lww_register.Mem (In)
open L

let return = Lwt.return

let info = Irmin_unix.info

let conf () = Store.Repo.v (Irmin_mem.config ())

let path = [ "tmp"; "lww" ]

let test_read_write () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  read t ~path >>= fun x ->
  assert (x = None);
  write ~info:(info "writing 1") t ~path 1 >>= fun () ->
  write ~info:(info "writing 3") t ~path 3 >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = Some 3);
  return ()

let test_clone_merge () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  Store.clone ~src:t ~dst:"br1" >>= fun b ->
  write ~info:(info "writing 5 to master") t ~path 5 >>= fun () ->
  write ~info:(info "writing 10 to clone") b ~path 10 >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = Some 5);
  read b ~path >>= fun x ->
  assert (x = Some 10);
  (Store.merge_into ~info:(info "merging clone into master") b ~into:t
   >>= function
   | Error _ -> failwith "error"
   | Ok _ -> read t ~path)
  >>= fun x ->
  assert (x = Some 10);
  return ()

let test_branch_merge () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  Store.of_branch r "br2" >>= fun b ->
  write ~info:(info "writing 6 to branch") b ~path 6 >>= fun () ->
  write ~info:(info "writing 3 to master") t ~path 3 >>= fun () ->
  read t ~path >>= fun x ->
  assert (x = Some 3);
  read b ~path >>= fun x ->
  assert (x = Some 6);
  (Store.merge_into ~info:(info "merging branch into master") b ~into:t
   >>= function
   | Error _ -> failwith "error"
   | Ok _ -> read t ~path)
  >>= fun x ->
  assert (x = Some 3);
  return ()

let test () = test_read_write () >>= test_clone_merge >>= test_branch_merge
