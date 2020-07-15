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
module C = Irmin_containers.Counter.Mem

let path = [ "tmp"; "counter" ]

let config () = C.Store.Repo.v (Irmin_mem.config ())

let test_inc _ () =
  config () >>= C.Store.master >>= fun t ->
  C.inc ~path t >>= fun () ->
  C.read ~path t
  >|= Alcotest.(check int64) "checked - increment without using by" 1L
  >>= fun () ->
  C.inc ~by:2L ~path t >>= fun () ->
  C.read ~path t >|= Alcotest.(check int64) "checked - increment using by" 3L

let test_dec _ () =
  config () >>= C.Store.master >>= fun t ->
  C.dec ~path t >>= fun () ->
  C.read ~path t
  >|= Alcotest.(check int64) "checked - decrement without using by" 2L
  >>= fun () ->
  C.dec ~by:2L ~path t >>= fun () ->
  C.read ~path t >|= Alcotest.(check int64) "checked - decrement using by" 0L

let test_clone_merge _ () =
  config () >>= C.Store.master >>= fun t ->
  C.inc ~by:5L ~path t >>= fun () ->
  C.Store.clone ~src:t ~dst:"cl" >>= fun b ->
  C.inc ~by:2L ~path b >>= fun () ->
  C.dec ~by:4L ~path t >>= fun () ->
  C.read ~path t >|= Alcotest.(check int64) "checked - value of master" 1L
  >>= fun () ->
  C.read ~path b >|= Alcotest.(check int64) "checked - value of clone" 7L
  >>= fun () ->
  C.Store.merge_into ~info:Irmin.Info.none b ~into:t
  >>= (function Error _ -> failwith "merging error" | Ok _ -> C.read t ~path)
  >|= Alcotest.(check int64) "checked - value of master after merging" 3L

let test_branch_merge _ () =
  config () >>= fun r ->
  C.Store.of_branch r "b1" >>= fun b1 ->
  C.Store.of_branch r "b2" >>= fun b2 ->
  C.Store.of_branch r "b3" >>= fun b3 ->
  C.Store.of_branch r "b4" >>= fun b4 ->
  C.inc ~by:5L ~path b1 >>= fun () ->
  C.dec ~by:2L ~path b2 >>= fun () ->
  C.Store.merge_into ~info:Irmin.Info.none b1 ~into:b3
  >>= (function
        | Error _ -> failwith "error while merging b1 into b3"
        | Ok _ -> C.Store.merge_into ~info:Irmin.Info.none b2 ~into:b3)
  >>= (function
        | Error _ -> failwith "error while merging b2 into b3"
        | Ok _ -> C.Store.merge_into ~info:Irmin.Info.none b2 ~into:b4)
  >>= (function
        | Error _ -> failwith "error while merging b2 into b4"
        | Ok _ -> C.Store.merge_into ~info:Irmin.Info.none b1 ~into:b4)
  >>= (function
        | Error _ -> failwith "error while merging b1 into b4"
        | Ok _ -> C.read ~path b3)
  >|= Alcotest.(check int64) "checked - value of b3" 3L
  >>= fun () ->
  C.read ~path b4 >|= Alcotest.(check int64) "checked - value of b4" 3L

let test_cases =
  [
    ( "counter operations",
      [
        Alcotest_lwt.test_case "Increment" `Quick test_inc;
        Alcotest_lwt.test_case "Decrement" `Quick test_dec;
      ] );
    ( "counter store operations",
      [
        Alcotest_lwt.test_case "Clone and merge" `Quick test_clone_merge;
        Alcotest_lwt.test_case "Branch and merge" `Quick test_branch_merge;
      ] );
  ]

(* let () = Lwt_main.run @@ Alcotest_lwt.run "Counter" test_cases *)
