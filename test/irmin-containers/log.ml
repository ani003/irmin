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

module String = struct
  type t = string

  let t = Irmin.Type.string
end

module M = Irmin_containers.Log.Mem (String)
open M

let return = Lwt.return

let info = Irmin_unix.info

let conf () = Store.Repo.v (Irmin_mem.config ())

let path = [ "tmp"; "log" ]

let test_append_read () =
  conf () >>= fun r ->
  Store.master r >>= fun t ->
  read_all t ~path >>= fun l ->
  assert (l = []);
  append ~info:(info "appending to master") t ~path "master.1" >>= fun () ->
  append ~info:(info "appending to master") t ~path "master.2" >>= fun () ->
  (get_cursor t ~path >>= function
   | None -> failwith "error"
   | Some c -> return c)
  >>= fun c ->
  read c ~num_items:1 >>= fun (l, c) ->
  ( assert (l = [ "master.2" ]);
    match c with None -> failwith "error" | Some c -> return c )
  >>= fun c ->
  read c ~num_items:1 >>= fun (l, _) ->
  assert (l = [ "master.1" ]);
  (get_cursor t ~path >>= function
   | None -> failwith "error"
   | Some c -> return c)
  >>= fun c ->
  read c ~num_items:10 >>= fun (l, _) ->
  assert (l = [ "master.2"; "master.1" ]);
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
  read_all b ~path >>= fun l ->
  assert (l = [ "clone.2"; "clone.1"; "master.2"; "master.1" ]);
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
