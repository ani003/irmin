(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open IrminTypes

let debug fmt = IrminMisc.debug "IO" fmt

let create len =
  let buffer = Cstruct.create len in
  let str = String.make len 'x' in
  Cstruct.blit_from_string str 0 buffer 0 len;
  { buffer }

let dump_buffer t =
  if IrminMisc.debug_enabled () then
    let debug = Cstruct.debug t.buffer in
    let str = Cstruct.to_string (Cstruct.shift t.buffer (-t.buffer.Cstruct.off)) in
    Printf.eprintf "%16s\027[33m[[ %s %S ]]\027[m\n" ""
      debug str

let set t len fn c =
  debug "set len:%d" len;
  dump_buffer t;
  fn t.buffer 0 c;
  t.buffer <- Cstruct.shift t.buffer len

let set_char t c =
  set t 1 Cstruct.set_char c

let set_uint8 t c =
  set t 1 Cstruct.set_uint8 c

let set_uint16 t c =
  set t 2 Cstruct.BE.set_uint16 c

let set_uint32 t c =
  set t 4 Cstruct.BE.set_uint32 c

let set_uint64 t c =
  set t 8 Cstruct.BE.set_uint64 c

let set_string t str =
  let len = String.length str in
  set t len (fun _ _ _ ->
      Cstruct.blit_from_string str 0 t.buffer 0 len;
    ) str

let get t n fn =
  let i = fn t.buffer 0 in
  t.buffer <- Cstruct.shift t.buffer n;
  i

let get_char t =
  get t 1 Cstruct.get_char

let get_uint8 t =
  get t 1 Cstruct.get_uint8

let get_uint16 t =
  get t 2 Cstruct.BE.get_uint16

let get_uint32 t =
  get t 4 Cstruct.BE.get_uint32

let get_uint64 t =
  get t 8 Cstruct.BE.get_uint64

let get_string t len =
  let str = String.create len in
  get t len (fun _ _ ->
      Cstruct.blit_to_string t.buffer 0 str 0 len;
    );
  str

module OCamlList = List
module OCamlString = String

exception Parse_error of string

let parse_error_buf buf fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "\027[31mParse error:\027[m %s\n" str;
      dump_buffer buf;
      raise (Parse_error str)
    ) fmt

let parse_error fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "\027[31mParse error:\027[m %s\n" str;
      raise (Parse_error str)
    ) fmt

module List (E: BASE) = struct

  let debug fmt = IrminMisc.debug "IO-LIST" fmt

  type t = E.t list

  let rec compare l1 l2 = match l1, l2 with
    | []    , []     -> 0
    | h1::t1, h2::t2 -> if E.compare h1 h2 = 0 then 0 else compare t1 t2
    | _::_  , []     -> 1
    | []    , _::_   -> -1

  let pretty t =
    String.concat "\n" (OCamlList.rev (OCamlList.rev_map E.pretty t))

  let equal l1 l2 =
    compare l1 l2 = 0

  let hash = Hashtbl.hash

  let to_json t =
    `A (OCamlList.rev (OCamlList.rev_map E.to_json t))

  let of_json = function
    | `A l -> OCamlList.rev (List.rev_map E.of_json l)
    | _    -> parse_error "List.of_json"

  let sizeof l =
    debug "sizeof";
    4 +
    List.fold_left (fun acc e ->
        acc + E.sizeof e
      ) 0 l

  let get buf =
    debug "get";
    let keys = Int32.to_int (get_uint32 buf) in
    let rec aux acc i =
      if i <= 0 then OCamlList.rev acc
      else
        let t = E.get buf in
        aux (t :: acc) (i-1) in
    if keys = 0 then []
    else aux [] keys

  let set buf t =
    debug "set %s" (pretty t);
    let len = Int32.of_int (List.length t) in
    let () = set_uint32 buf len in
    List.iter (E.set buf) t

end

module Option (E: BASE) = struct

  let debug fmt = IrminMisc.debug "IO-OPTION" fmt

  module L = List(E)

  type t = E.t option

  let compare o1 o2 = match o1, o2 with
    | None   , None    -> 0
    | Some _ , None    -> 1
    | None   , Some _  -> -1
    | Some e1, Some e2 -> E.compare e1 e2

  let pretty = function
    | None   -> "<none>"
    | Some e -> E.pretty e

  let equal o1 o2 =
    compare o1 o2 = 0

  let hash = function
    | None   -> 0
    | Some e -> E.hash e

  let to_json = function
    | None   -> `Null
    | Some e -> E.to_json e

  let of_json = function
    | `Null -> None
    | j     -> Some (E.of_json j)

  let sizeof t =
    debug "sizeof";
    match t with
    | None   -> 4
    | Some e -> 4 + E.sizeof e

  let get buf =
    debug "get";
    let l = L.get buf in
    match l with
    | []  -> None
    | [e] -> Some e
    | _   -> parse_error_buf buf "Option.get"

  let set buf t =
    debug "set %s" (pretty t);
    let l = match t with
      | None   -> []
      | Some e -> [e] in
    L.set buf l

end

module Pair (K: BASE) (V: BASE) = struct

  let debug fmt = IrminMisc.debug "IO-PAIR" fmt

  type t = K.t * V.t

  let compare (k1,v1) (k2,v2) =
    match K.compare k1 k2 with
    | 0 -> V.compare v1 v2
    | i -> i

  let pretty (key, value) =
    Printf.sprintf "%s:%s" (K.pretty key) (V.pretty value)

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash (key, value) =
    Hashtbl.hash (K.hash key, V.hash value)

  let to_json (key, value) =
    `O [ ("tag", K.to_json key);
         ("key", V.to_json value)]

  let of_json = function
    | `O l ->
      let key =
        try OCamlList.assoc "tag" l
        with Not_found -> parse_error "Product.of_json: missing tag" in
      let value =
        try OCamlList.assoc "key" l
        with Not_found -> parse_error "Product.of_json: missing key" in
      (K.of_json key, V.of_json value)
    | _ -> parse_error "Product.of_json: not an object"

  let sizeof (key, value) =
    let k = K.sizeof key in
    let v = V.sizeof value in
    debug "sizeof k:%d v:%d" k v;
    k+v

  let get buf =
    debug "get";
    let tag = K.get buf in
    let key = V.get buf in
    (tag, key)

  let set buf (key, value as t) =
    debug "set %s" (pretty t);
    dump_buffer buf;
    K.set buf key;
    V.set buf value

end

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module String  (S: STRINGABLE) = struct

  let debug fmt = IrminMisc.debug "IO-STRING" fmt

  type t = S.t

  let compare s1 s2 = String.compare (S.to_string s1) (S.to_string s2)

  let pretty s =
    Printf.sprintf "%S" (S.to_string s)

  let equal s1 s2 =
    compare s1 s2 = 0

  let hash = Hashtbl.hash

  let to_json t =
    IrminJSON.of_string (S.to_string t)

  let of_json j =
    S.of_string (IrminJSON.to_string j)

  let sizeof s =
    debug "sizeof";
    4 + String.length (S.to_string s)

  let get buf =
    debug "get";
    let len = get_uint32 buf in
    let str = get_string buf (Int32.to_int len) in
    S.of_string str

  let set buf t =
    debug "set %s" (pretty t);
    let str = S.to_string t in
    let len = String.length str in
    set_uint32 buf (Int32.of_int len);
    set_string buf str

end

module Lwt_channel = struct

  let debug fmt = IrminMisc.debug "IO-LWT" fmt

  type t = {
    fd  : Lwt_unix.file_descr;
    name: string;
  }

  let name t = t.name

  let channel t = t.fd

  let close t = Lwt_unix.close t.fd

  let read_string t len =
    debug "read_string %s len:%d" t.name len;
    let str = OCamlString.create len in
    let rec rread fd str ofs len =
      lwt n = Lwt_unix.read fd str ofs len in
      debug " ... read_string n:%d" n;
      if n = 0 then raise_lwt End_of_file
      else if n < len then rread fd str (ofs + n) (len - n)
      else Lwt.return () in
    lwt () = rread t.fd str 0 len in
    Lwt.return str

  let read_buf t len =
    debug "read_buf %s len:%d" t.name len;
    let bufIO = create len in
    let buf = bufIO.buffer.Cstruct.buffer in
    let rec rread fd buf ofs len =
      debug "rread ofs=%d len=%d" ofs len;
      lwt n = Lwt_bytes.read fd buf ofs len in
      if n = 0 then raise_lwt End_of_file
      else if n < len then rread fd buf (ofs + n) (len - n)
      else Lwt.return () in
    lwt () = rread t.fd buf 0 len in
    dump_buffer bufIO;
    Lwt.return bufIO

  let write_string t str =
    debug "write_string %s %S" t.name str;
    let rec rwrite fd str ofs len =
      lwt n = Lwt_unix.write fd str ofs len in
      if n = 0 then raise_lwt End_of_file
      else if n < len then rwrite fd str (ofs + n) (len - n)
      else Lwt.return () in
    rwrite t.fd str 0 (OCamlString.length str)

  let write_buf t buf =
    debug "write_buf %s" t.name;
    dump_buffer buf;
    let rec rwrite fd buf ofs len =
      debug " ... write_buf %d" len;
      lwt n = Lwt_bytes.write fd buf ofs len in
      if n = 0 then raise_lwt End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else Lwt.return () in
    let ba = buf.buffer.Cstruct.buffer in
    rwrite t.fd ba 0 (Bigarray.Array1.dim ba)

  let read_length t =
    debug "read_length %S" t.name;
    lwt str = read_string t 4 in
    let len = EndianString.BigEndian.get_int32 str 0 in
    debug " ... read_length: %ld" len;
    Lwt.return (Int32.to_int len)

  let write_length t len =
    debug "write_length %s %dl" t.name len;
    let str = OCamlString.create 4 in
    EndianString.BigEndian.set_int32 str 0 (Int32.of_int len);
    lwt () = write_string t str in
    debug " ... write_length";
    Lwt.return ()

  let write_unit t =
    write_string t "U"

  let read_unit t =
    lwt str = read_string t 1 in
    assert (str = "U");
    Lwt.return ()

  let create fd name = { fd; name }

  let unix_socket_server ~limit file =
    debug "unix-socker-server %s" file;
    let fd = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    Lwt_unix.bind fd (Lwt_unix.ADDR_UNIX file);
    Lwt_unix.listen fd limit;
    create fd file

  let unix_socket_client file =
    debug "unix-socket-client %s" file;
    let fd = Lwt_unix.(socket PF_UNIX SOCK_STREAM 0) in
    lwt () = Lwt_unix.connect fd (Lwt_unix.ADDR_UNIX file) in
    Lwt.return (create fd file)

end

module type CHANNEL = sig
  include BASE
  type channel = Lwt_channel.t
  val read_fd: channel -> t Lwt.t
  val write_fd: channel -> t -> unit Lwt.t
end

module Channel (B: BASE) = struct

  let debug fmt = IrminMisc.debug "IO-FD" fmt

  include B

  type channel = Lwt_channel.t

  let read_fd fd =
    debug "read_fd %s" (Lwt_channel.name fd);
    lwt len = Lwt_channel.read_length fd in
    debug " ... read_fd len:%d" len;
    lwt buf = Lwt_channel.read_buf fd len in
    debug " ... read_buf";
    dump_buffer buf;
    Lwt.return (B.get buf)

  let write_fd fd t =
    let len = B.sizeof t in
    debug "write_fd %s len:%d %s" (Lwt_channel.name fd) len (B.pretty t);
    let buf = create len in
    B.set buf t;
    lwt () = Lwt_channel.write_length fd len in
    lwt () = Lwt_channel.write_buf fd buf in
    debug " ... write_fd";
    dump_buffer buf;
    Lwt.return ()

end
