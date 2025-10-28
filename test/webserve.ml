(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* HTTPS 404 echo server – echoes back the request's headers in a 404

   ocamlfind ocamlopt -package unix,threads,b0.std,bytesrw.tls \
     -linkpkg webserve.ml -o webserve
   ./webserve > /tmp/cert.pem
   curl --cacert /tmp/cert.pem https://localhost:4443
*)

open B0_std
open Result.Syntax
open Bytesrw

(* HTTP matters *)

let read_http_request_headers recv =
  let rec loop b st recv = match Bytes.Reader.read recv with
  | slice when Bytes.Slice.is_eod slice -> Buffer.contents b
  | slice ->
      let bytes = Bytes.Slice.bytes slice in
      let first = Bytes.Slice.first slice in
      let last = first + Bytes.Slice.length slice - 1 in
      let i = ref first in
      while (!i <= last || !st <> 4) do
        let c = Bytes.get bytes !i in
        if c = '\r' then (if !st = 0 || !st = 2 then incr st else st := 0) else
        if c = '\n' then (if !st = 1 || !st = 3 then incr st else st := 0) else
        (st := 0);
        incr i;
      done;
      if !st <> 4 then (Bytes.Slice.add_to_buffer b slice; loop b st recv) else
      let eoh = Bytes.Slice.subrange_or_eod ~last:(!i - first - 3) slice in
      let body_start = Bytes.Slice.subrange_or_eod ~last:!i slice in
      Bytes.Reader.push_back recv body_start; (* not read by this program *)
      Bytes.Slice.add_to_buffer b eoh;
      Buffer.contents b
  in
  loop (Buffer.create 255) (ref 0 (* Used to detect CRLFCRLF *)) recv

let http_404_text_response text =
  Fmt.str "HTTP/1.1 404 Not found\r\n\
           content-type: text/plain\r\n\
           connection: close\r\n\r\n%s" text

let http_perform_exchange ~peer:(send, recv) =
  (* Note: this function is oblivious of transport encryption *)
  let headers = read_http_request_headers recv in
  Log.info (fun m -> m ~header:"" "@[<v>%a@]" Fmt.lines headers);
  Bytes.Writer.write_string send (http_404_text_response headers);
  Bytes.Writer.write_eod send;
  Ok ()

(* Certificate matters *)

let own_certs ~own_cert ~own_key = match own_cert with
| None ->
    Log.warn (fun m -> m "No certificate provided. Generating one.");
    let* private_key = match own_key with
    | None -> Ok None
    | Some keyfile ->
        Result.map Option.some @@
        Bytesrw_tls.X509_certchain.Private_key.read_pem_file keyfile
    in
    let* cert, k = Bytesrw_tls.X509_certchain.self_signed ?private_key () in
    let cert_pem = Bytesrw_tls.X509_certchain.to_pem cert in
    Log.stdout (fun m -> m "@[Server certificate:@,%a@]" Fmt.lines cert_pem);
    Ok [cert, k]
| Some certfile ->
    match own_key with
    | None ->
        Fmt.error "Missing certificate private key (use %a)"
          Fmt.code "--own-key"
    | Some keyfile ->
        let* cert = Bytesrw_tls.X509_certchain.read_pem_file certfile in
        let* k = Bytesrw_tls.X509_certchain.Private_key.read_pem_file keyfile in
        Ok [cert, k]

(* Networking matters *)

let with_tls_net_io ~conf ~peer:fd f =
  try
    Bytesrw_tls.for_server_socket conf ~peer:fd @@ fun info ~peer ->
    Log.info (fun m -> m ~header:"" "%a" Bytesrw_tls.Info.pp info);
    Log.debug
      (fun m -> m ~header:"" "%a" Bytesrw_tls.Info.pp_peer_cert_pem info);
    f ~peer
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let serve_peer ~conf ~peer ~peer_addr:addr () =
  Log.if_error ~use:() @@
  let finally () = Os.Fd.close_noerr peer in
  Result.join @@ Fun.protect ~finally @@ fun () ->
  with_tls_net_io ~conf ~peer @@ fun ~peer ->
  http_perform_exchange ~peer

let serve ~endpoint:ep ~own_cert ~own_key =
  Log.debug (fun m -> m "%s" (Bytesrw_tls.backend_info ()));
  Result.map_error (fun e -> Fmt.str "%a %s" Fmt.puterr () e) @@ Result.join @@
  let* conf =
    let* own_certs = own_certs ~own_cert ~own_key in
    let alpn_protocols = ["http/1.1"] in
    Bytesrw_tls.Conf.make ~own_certs ~alpn_protocols Server
  in
  Result.join @@ Os.Socket.with_listening_endpoint ep SOCK_STREAM @@
  fun listen addr ->
  let ep = Net.Endpoint.with_port_of_sockaddr addr ep in
  Log.stderr (fun m -> m "Listening on https://%a" Net.Endpoint.pp ep);
  let rec serve_loop listen = match Os.Socket.accept ~cloexec:true listen with
  | Error _ as e -> e
  | Ok (peer, peer_addr) ->
      Log.info (fun m -> m ~header:"" "connect: %a" Fmt.sockaddr addr);
      (* Don't do that at home, throttle and/or use a thread pool *)
      ignore (Thread.create (serve_peer ~conf ~peer ~peer_addr) ());
      serve_loop listen
  in
  serve_loop listen

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let cmd =
  let doc = "Echo HTTP requests" in
  Cmd.make (Cmd.info "webserve" ~version:"%%VERSION%%" ~doc) @@
  let+ () = B0_std_cli.set_log_level ()
  and+ endpoint = B0_std_cli.net_endpoint_listener ~default_port:4443 ()
  and+ own_cert =
    let doc = "$(docv) a PEM file with the certificate chain of your server" in
    let docv = "CERT.pem" in
    Arg.(value & opt (some filepath) None & info ["own-cert"] ~doc ~docv)
  and+ own_key =
    let doc =
      "$(docv) a PEM file with the private key of the first certificate \
       in the chain given in $(b,--own-cert)"
    in
    let docv = "KEY.pem" in
    Arg.(value & opt (some filepath) None & info ["own-key"] ~doc ~docv)
  in
  serve ~endpoint ~own_cert ~own_key

let main () = Cmd.eval_result cmd
let () = if !Sys.interactive then () else exit (main ())
