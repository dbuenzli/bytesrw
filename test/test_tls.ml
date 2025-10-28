(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* This tests [Bytesrw_tls] by testing mutual TLS. Both clients and servers
   run as threads of the same process. *)

open B0_std
open Result.Syntax
open B0_testing
open Bytesrw

let pp_entity = Fmt.st [`Fg `Yellow]
let log ~name fmt = Test.log ("[%a] @[" ^^ fmt ^^ "@]") pp_entity name

module Client = struct
  type t =
    { name : string;
      owncert : Bytesrw_tls.X509_certchain.own option }

  let make ?(with_cert = true) ~name () =
    let* owncert =
      if not with_cert then Ok None else
      Result.map Option.some (Bytesrw_tls.X509_certchain.self_signed ~name ())
    in
    Ok { name; owncert }

  let get_cert c = fst (Option.get (c.owncert))
  let tls_conf c ~server_cert =
    let own_certs = match c.owncert with
    | Some cert -> [cert] | None -> []
    in
    let trusted_certs = match server_cert with
    | None -> [] | Some cert -> [cert]
    in
    Bytesrw_tls.Conf.make ~trusted_certs ~own_certs Client

  let handle_run_result ~name ~success = function
  | Error e ->
      log ~name "%a" Fmt.text e;
      if not success then Test.pass () else
      Test.fail "%a %a was not supposed to error" Fmt.puterr () pp_entity name
  | Ok () ->
      if success then Test.pass () else
      Test.fail "%a %a was supposed to error" Fmt.puterr () pp_entity name

  let run c
      ?(ignore_server_cert = false) ~server:(server_ep, server_cert) ~success
      ()
    =
    let run () =
      let name = c.name in
      let server_cert = if ignore_server_cert then None else Some server_cert in
      let peer_hostname = match server_ep with
      | `Host (h, _) -> h | _ -> assert false
      in
      handle_run_result ~name ~success @@
      let* conf = tls_conf c ~server_cert in
      Result.join @@ Os.Socket.with_connected_endpoint server_ep SOCK_STREAM @@
      fun peer addr ->
      Result.join @@ Bytesrw_tls.for_client_socket conf ~peer_hostname ~peer @@
      fun info ~peer:(send, recv) ->
      try
        log ~name "Connected: %a" Bytesrw_tls.Info.pp info;
        Bytes.Writer.write_string send (Fmt.str "Hey I'am %s" c.name);
        Bytes.Writer.write_eod send;
        log ~name "Received: %s" (Bytes.Reader.to_string recv);
        Ok ()
      with
      | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e
    in
    Thread.create run ()
end

module Server = struct
  let serve_peer ~conf ~peer ~peer_addr () =
    let name = "server" in
    let handle_error = function
    | Error e -> log ~name "Serving peer: %a" Fmt.text e
    | Ok () -> ()
    in
    handle_error @@
    Result.join @@ Bytesrw_tls.for_server_socket conf ~peer @@
    fun info ~peer:(send, recv) ->
    try
      log ~name "New peer %a" Bytesrw_tls.Info.pp info;
      let msg = Bytes.Reader.to_string recv in
      log ~name "Peer said: %s" msg;
      Bytes.Writer.write_string send (Fmt.str "You said: %s" msg);
      Bytes.Writer.write_eod send;
      Ok ()
    with
    | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

  let run ~accept_count ~trusted_clients:trusted_certs =
    let* server_own = Bytesrw_tls.X509_certchain.self_signed () in
    let* conf =
      let own_certs = [server_own] and verify_peer = true in
      Bytesrw_tls.Conf.make ~trusted_certs ~own_certs ~verify_peer Server
    in
    let ep = `Host ("localhost", 0 (* allocate a port *)) in
    let* listen, close, addr = Os.Socket.listen_endpoint ep SOCK_STREAM in
    let () = Sys.set_signal Sys.sigpipe Signal_ignore in
    let serve () =
      let rec serve_loop accept_count listen =
        if accept_count = 0 then () else
        match Os.Socket.accept ~cloexec:true listen with
        | Error _ as e -> Log.if_error ~use:() e
        | Ok (peer, peer_addr) ->
            ignore (Thread.create (serve_peer ~conf ~peer ~peer_addr) ());
            serve_loop (accept_count - 1) listen
      in
      let finally () = if close then Os.Fd.close_noerr listen in
      Fun.protect ~finally (fun () -> serve_loop accept_count listen)
    in
    let tid = Thread.create serve () in
    let ep = Net.Endpoint.with_port_of_sockaddr addr ep in
    Ok ((ep, fst server_own), tid)
end

let test =
  Test.test "Mutual TLS" @@ fun () ->
  Test.log "%s" (Bytesrw_tls.backend_info ());
  Result.error_to_failure @@
  let* alice = Client.make ~name:"alice" () in
  let* bob = Client.make ~name:"bob" () in
  let* mallory = Client.make ~name:"mallory" () in
  let* trudy = Client.make ~with_cert:false ~name:"trudy" () in
  let* victor = Client.make ~name:"victor" () in
  let* host, server =
    let trusted_clients = Client.[alice; bob; victor] in
    let trusted_clients = List.map Client.get_cert trusted_clients in
    Server.run ~accept_count:5 ~trusted_clients
  in
  let alice = Client.run alice ~server:host ~success:true () in
  let mallory =
    let success = false (* because he's not trusted by the server. *)in
    Client.run mallory ~server:host ~success () in
  let trudy =
    let success = false (* because he has no cert *) in
    Client.run trudy ~server:host ~success ()
  in
  let bob = Client.run bob ~server:host ~success:true () in
  let victor =
    let success = false (* becase he can't verify the server *) in
    Client.run victor ~ignore_server_cert:true ~server:host ~success ()
  in
  List.iter Thread.join [server; alice; mallory; bob; trudy; victor];
  Gc.full_major ();
  Ok ()

let main () = Test.main @@ fun () -> Test.autorun ()
let () = if !Sys.interactive then () else exit (main ())
