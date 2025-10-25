(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* GET https://Sys.argv.(1)/

   ocamlfind ocamlopt -package unix,bytesrw.tls -linkpkg min_tls.ml
   ./a.out

   See also webfetch.ml for a more realistic example. *)

open Bytesrw

let ( let* ) = Result.bind
let str = Printf.sprintf

let http_get_root host =
  str "GET / HTTP/1.1\r\nhost: %s\r\nconnection:close\r\n\r\n" host

let with_connected_endpoint (`Host (host, port)) f =
  try
    Sys.set_signal Sys.sigpipe Signal_ignore;
    let addr = (Unix.gethostbyname host).Unix.h_addr_list.(0) in
    let addr = Unix.ADDR_INET (addr, port) in
    let domain = Unix.domain_of_sockaddr addr and stype = Unix.SOCK_STREAM in
    let sock = Unix.socket ~cloexec:true domain stype 0 in
    let finally () = try Unix.close sock with Unix.Unix_error _ -> () in
    Fun.protect ~finally @@ fun () -> Unix.connect sock addr; Ok (f sock)
  with
  | Sys_error s -> Error s
  | Not_found -> Error (str "%s: server not found" host)
  | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)

let endpoint_of_argv ~port = match Sys.argv with
| [| _ |] -> Ok (`Host ("example.org", port))
| [| _; hostname |] -> Ok (`Host (hostname, port))
| _ -> Error (str "Usage: %s [HOSTNAME]" Sys.executable_name)

let main () =
  let handle_exit = function Ok () -> 0 | Error e -> prerr_endline e; 1 in
  handle_exit @@
  let* `Host (peer_hostname, _) as ep = endpoint_of_argv ~port:443 in
  let* conf = Bytesrw_tls.Conf.make Client in
  Result.join @@ with_connected_endpoint ep @@ fun fd ->
  Result.join @@ Bytesrw_tls.for_client_socket conf ~peer_hostname ~peer:fd @@
  fun _info ~peer:(send, recv) ->
  try
    (* Note: we don't write eod, shutting down trips out some servers *)
    Bytes.Writer.write_string send (http_get_root peer_hostname);
    print_endline (Bytes.Reader.to_string recv);
    Ok ()
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let () = if !Sys.interactive then () else exit (main ())
