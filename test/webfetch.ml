(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* GET an HTTP(S) URL via HTTP/1.1

   ocamlfind ocamlopt -package unix,b0.std,bytesrw.unix,bytesrw.tls \
     -linkpkg webfetch.ml -o webfetch
   ./webfetch http://example.org
   ./webfetch https://example.org

   See also the min_tls.ml example for a more bare-bones example. *)

open B0_std
open Result.Syntax
open Bytesrw

(* HTTP matters *)

let http_prepare_get_request url =
  Result.map_error (Fmt.str "%s: %s" url) @@
  let* tls = match B0_url.scheme url with
  | None | Some "https" -> Ok true
  | Some "http" -> Ok false
  | Some other -> Fmt.error "URL scheme %s unknown" other
  in
  let* authority = match B0_url.authority url with
  | Some authority -> Ok authority | None -> Error "No authority found"
  in
  let host = B0_url.Authority.host authority in
  let port = match B0_url.Authority.port authority with
  | Some port -> port | None -> if tls then 443 else 80
  in
  let target = Option.value ~default:"/" (B0_url.target url) in
  let target = B0_url.Percent.encode `Uri target in
  let request = Fmt.str
      "GET %s HTTP/1.1\r\nhost: %s\r\nconnection: close\r\n\r\n" target host
  in
  Ok (`Host (host, port), tls, request)

let http_perform_exchange ~peer:(send, recv) ~request ~response =
  (* Note: this function is oblivious of transport encryption
     Note: we don't write eod, shutting down trips out some servers *)
  Log.info (fun m -> m ~header:"" "%s" request);
  Bytes.Writer.write_string send request;
  Bytes.Writer.write_reader response ~eod:true recv;
  Ok ()

(* Certificate matters *)

let http_tls_conf ~cacert ~insecure =
  let* trusted_certs = match cacert with
  | None -> Ok None
  | Some file ->
      match Bytesrw_tls.X509_certchain.read_pem_file file with
      | Error _ as e -> e
      | Ok bundle -> Ok (Some [bundle])
  in
  let verify_peer = if insecure then false else true in
  let alpn_protocols = ["http/1.1"] in
  Bytesrw_tls.Conf.make ~alpn_protocols ?trusted_certs ~verify_peer Client

(* Networking matters *)

let with_unencrypted_net_io ~peer:fd f =
  try
    let send = Bytesrw_unix.bytes_writer_of_socket_fd fd in
    let recv = Bytesrw_unix.bytes_reader_of_fd fd in
    Ok (f ~peer:(send, recv))
  with
  | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let with_tls_net_io conf ~pem_log ~peer_hostname ~peer:fd f =
  try
    Bytesrw_tls.for_client_socket conf ~peer_hostname ~peer:fd @@
    fun info ~peer ->
    Log.info (fun m -> m ~header:"" "%a" Bytesrw_tls.Info.pp info);
    Log.debug (fun m ->
        let pp = match pem_log with
        | true -> Bytesrw_tls.Info.pp_peer_cert_pem
        | false -> Bytesrw_tls.Info.pp_peer_cert
        in
        m ~header:"" "%a" pp info);
    f ~peer
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let fetch ~url ~cacert ~insecure ~pem_log =
  Log.debug (fun m -> m "%s" (Bytesrw_tls.backend_info ()));
  Result.map_error (fun e -> Fmt.str "%a %s" Fmt.puterr () e) @@ Result.join @@
  let* `Host (host, _) as ep, tls, request = http_prepare_get_request url in
  Os.Socket.with_connected_endpoint ep SOCK_STREAM @@ fun peer _addr ->
  let* net_io =
    if not tls then Ok (with_unencrypted_net_io ~peer) else
    let* conf = http_tls_conf ~cacert ~insecure in
    Ok (with_tls_net_io ~pem_log conf ~peer_hostname:host ~peer)
  in
  Result.join @@ net_io @@ fun ~peer ->
  let response = Bytesrw_unix.bytes_writer_of_fd Unix.stdout in
  http_perform_exchange ~peer ~request ~response

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Fetch an HTTP URL" in
  let envs = [Cmd.Env.info "SSL_CERT_FILE" ~doc:"See $(b,--cacert) option."] in
  Cmd.make (Cmd.info "webfetch" ~version:"%%VERSION%%" ~doc ~envs) @@
  let+ () = B0_std_cli.set_log_level ()
  and+ url =
    let doc = "The URL to fetch" in
    Arg.(required & pos 0 (some string) None & info [] ~doc ~docv:"URL")
  and+ cacert =
    let doc =
      "$(docv) a PEM file with the certificates of certification \
       authorities."
    in
    let absent = "System lookup or $(b,SSL_CERT_FILE) env" in
    let docv = "CERT.pem" in
    Arg.(value & opt (some filepath) None & info ["cacert"] ~absent ~doc ~docv)
  and+ insecure =
    let doc = "Do not check the server certificate." in
    Arg.(value & flag & info ["insecure"] ~doc)
  and+ pem_log =
    let doc = "Log certificate info in PEM format." in
    Arg.(value & flag & info ["pem-log"] ~doc)
  in
  fetch ~url ~cacert ~insecure ~pem_log

let main () = Cmd.eval_result cmd
let () = if !Sys.interactive then () else exit (main ())
