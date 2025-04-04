(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Bytesrw

let http_get_request url =
  Result.map_error (Fmt.str "%s: %s" url) @@
  let* tls =  match B0_url.scheme url with
  | None | Some "https" -> Ok true | Some "http" -> Ok false
  | Some other -> Fmt.error "Scheme %s unknown" other
  in
  let authority = B0_url.authority url in
  let* authority = Option.to_result ~none:"No authority found" authority in
  let host = B0_url.Authority.host authority in
  let port = match B0_url.Authority.port authority with
  | None -> if tls then 443 else 80 | Some port -> port
  in
  let target = Option.value ~default:"/" (B0_url.target url) in
  let target = B0_url.Percent.encode `Uri target in
  let request = "GET " ^ target ^ " HTTP/1.1\r\n\r\n" (* host *) in
  Ok (`Host (host, port), tls, request)

let connect fd addr =
  let* addr = Option.to_result ~none:"No address to connect" addr in
  match Unix.connect fd addr with
  | () -> Ok ()
  | exception Unix.Unix_error (e, _, _) ->
      Fmt.error "connect to %a: %s"
        Os.Socket.pp_sockaddr addr (Unix.error_message e)

let fetch_https_request fd req = failwith "TODO"
let fetch_http_request fd request =
  try
    let send = Bytesrw_unix.bytes_writer_of_fd fd in
    let recv = Bytesrw_unix.bytes_reader_of_fd fd in
    Bytes.Writer.write_string send request;
    Bytes.Writer.write_eod send;
    Ok (Bytes.Reader.to_string recv)
  with
  | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)

let fetch ~url =
  Result.map_error (fun e -> Fmt.str "%a %s" Fmt.puterr () e) @@
  let* `Host (host, port) as ep, tls, request = http_get_request url in
  let* addr, fd, close = Os.Socket.of_endpoint ep SOCK_STREAM in
  let finally () = if close then Os.Fd.close_noerr fd in
  Fun.protect ~finally @@ fun () ->
  let () = Unix.clear_nonblock fd in
  let* () = connect fd addr in
  Log.info (fun m -> m "@[<v>Sending:@,%a@]" Fmt.lines request);
  let* response =
    if tls
    then fetch_https_request fd url
    else fetch_http_request fd url
  in
  Ok (print_string response)

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Fetch an HTTP URL" in
  Cmd.make (Cmd.info "webfetch" ~version:"%%VERSION%%" ~doc) @@
  let+ () = B0_std_cli.set_log_level ()
  and+ url =
    let doc = "The URL to fetch" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")
  in
  fetch ~url

let main () = Cmd.eval_result cmd
let () = if !Sys.interactive then () else exit (main ())
