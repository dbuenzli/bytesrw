(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let ( let* ) = Result.bind

let string_to_hex s =
  if s = "" then "" else
  let hex_digit i = if i < 10 then Char.chr (48 + i) else Char.chr (87 + i) in
  let b = Bytes.create (String.length s * 3 - 1) in
  let max = String.length s - 1 in
  for i = 0 to max do
    let base = 3 * i in
    let byte = Char.code s.[i] in
    Bytes.set b (base    ) (hex_digit ((byte lsr 4) land 0xF));
    Bytes.set b (base + 1) (hex_digit (byte land 0xF));
    if i < max then Bytes.set b (base + 2) ':'
  done;
  Bytes.unsafe_to_string b

(* Low-level libraries *)

module Psa = Bytesrw_crypto.Psa (* Force linking to initalize PSA *)

let msg_of_psa_status ctx st =
  Printf.sprintf "%s: %s" ctx (Psa.Status.message st)

module Mbedtls = Bytesrw_tls__mbedtls

let msg_of_status ctx st =
  let sg = if st < 0 then "-" else "" in
  Printf.sprintf "%s: %s (%s0x%x)" ctx (Mbedtls.Status.message st) sg (abs st)

(* X.509 *)

module X509_certchain = struct
  type t = Mbedtls.x509_crt

  (* The C API is not super friendly to manipulate the certs individually
     with OCaml. To do that we go through their raw DER representation,
     which are kept by the underlying data structure. *)

  let internal_error st = (* Normally this should not be raised *)
    invalid_arg (msg_of_status "Bytesrw_tls.X509_certchain internal error" st)

  let internal_append_der_cert c der =
    let st = Mbedtls.x509_crt_parse_der c der in
    if not (Mbedtls.Status.is_ok st) then internal_error st

  let internal_cert_of_der der =
    let c = Mbedtls.x509_crt_init () in
    internal_append_der_cert c der; c

  let fold_der_certs f acc cs =
    let acc = ref acc in
    Mbedtls.x509_crt_iter_raw_der_chain cs (fun c -> acc := f !acc c); !acc

  let fold f acc cs =
    let conv f acc der = f acc (internal_cert_of_der der) in
    fold_der_certs (conv f) acc cs

  let concat = function
  | [] -> invalid_arg "Certificate list can't be empty"
  | [cs] -> cs
  | css ->
      let acc = Mbedtls.x509_crt_init () in
      let append cs =
        Mbedtls.x509_crt_iter_raw_der_chain cs (internal_append_der_cert acc);
      in
      List.iter append css; acc

  let leaf cs =
    let leaf = ref None in
    let iter c = match !leaf with None -> leaf := Some c | _ -> () in
    Mbedtls.x509_crt_iter_raw_der_chain cs iter;
    match !leaf with
    | None -> assert false | Some der -> internal_cert_of_der der

  (* PEM format *)

  let of_pem ?(file = "-") s =
    let c = Mbedtls.x509_crt_init () in
    let st = Mbedtls.x509_crt_parse c s in
    if Mbedtls.Status.is_ok st then Ok c else Error (msg_of_status file st)

  let to_pem cs =
    let add_cert b der =
      Buffer.add_string b "-----BEGIN CERTIFICATE-----";
      let base64 = Mbedtls.base64_encode der in
      for i = 0 to String.length base64 - 1 do
        if i mod 64 (* RFC 7468 *) = 0 then Buffer.add_char b '\n';
        Buffer.add_char b base64.[i];
      done;
      Buffer.add_string b "\n-----END CERTIFICATE-----\n";
      b
    in
    Buffer.contents (fold_der_certs add_cert (Buffer.create 255) cs)

  let read_pem_file file =
    (* [file] should be public material *)
    let read_file file =
      let read file ic = try Ok (In_channel.input_all ic) with
      | Sys_error e -> Error (Printf.sprintf "%s: %s" file e)
      in
      let binary_stdin () = In_channel.set_binary_mode In_channel.stdin true in
      try match file with
      | "-" -> binary_stdin (); read file In_channel.stdin
      | file -> In_channel.with_open_bin file (read file)
      with Sys_error e -> Error e
    in
    let* pem = read_file file in
    of_pem ~file pem

  (* DER format *)

  let of_der_certs ders =
    if ders = [] then invalid_arg "DER certificate list can't be empty" else
    let c = Mbedtls.x509_crt_init () in
    let add_der der =
      let st = Mbedtls.x509_crt_parse_der c der in
      if not (Mbedtls.Status.is_ok st)
      then failwith (msg_of_status "DER parse" st)
    in
    try List.iter add_der ders; Ok c with
    | Failure e -> Error e

  (* Formatter *)

  let pp_leaf ppf cs =
    let pp_name ppf n =
      Format.pp_print_as ppf 0 "\x1B[33m";
      Format.pp_print_string ppf n;
      Format.pp_print_as ppf 0 "\x1B[m";
    in
    let pp_field ppf (n, v) = Format.fprintf ppf "@[<1>%a:@ %s@]" pp_name n v in
    let stamp (y, m, d, hh, mm, ss) =
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02dZ" y m d hh mm ss
    in
    let fields =
      [ "serial", string_to_hex (Mbedtls.x509_crt_get_serial cs);
        "issuer", Mbedtls.x509_crt_get_issuer_name cs;
        "subject", Mbedtls.x509_crt_get_subject_name cs;
        "invalid-before", stamp (Mbedtls.x509_crt_get_valid_from cs);
        "invalid-after", " " ^ stamp (Mbedtls.x509_crt_get_valid_to cs);
        "is-ca", Bool.to_string (Mbedtls.x509_crt_get_is_ca cs)]
    in
    Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list pp_field) fields

  let pp ppf cs =
    let cs = List.rev (fold (Fun.flip List.cons) [] cs) in
    let pp_sep ppf () = Format.(pp_print_cut ppf (); pp_print_cut ppf ()) in
    Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list ~pp_sep pp_leaf) cs

  let pp_pem ppf cs =
    let lines = String.split_on_char '\n' (to_pem cs) in
    Format.fprintf ppf "@[<v>%a@]" Format.(pp_print_list pp_print_string) lines

  (* System CA certificates *)

  let root_cert_files =
    [ "/etc/ssl/certs/ca-certificates.crt";
      "/etc/ssl/cert.pem"; ]

  let system_ca_certs () = match Sys.getenv_opt "SSL_CERT_FILE" with
  | Some file -> Result.map Option.some (read_pem_file file)
  | None ->
      match Mbedtls.x509_crt_of_system_store () with
      | exception Failure e -> Error e
      | Some _ as chain -> Ok chain
      | None ->
          match List.filter Sys.file_exists root_cert_files with
          | [] -> Ok None
          | (file :: _) -> Result.map Option.some (read_pem_file file)

  (* Own certificates *)

  module Private_key = struct
    type t = Mbedtls.pk_context

    (* Note for now we piggyback on mbedtls's IO function as this allows
       to avoid the GC seeing the private data. Once we require 5.2 we
       could load via bigarrays and use the OCaml IO functions. *)

    let read_pem_file file =
      let pk = Mbedtls.pk_init () in
      let st = Mbedtls.pk_parse_keyfile pk file in
      if Mbedtls.Status.is_ok st then Ok pk else
      Error (msg_of_status file st)

    let write_pem_file file pk = match Mbedtls.pk_write_keyfile pk file with
    | exception Failure msg -> Error (Printf.sprintf "%s: %s" file msg)
    | st when not (Mbedtls.Status.is_ok st) -> Error (msg_of_status file st)
    | st -> Ok ()

    let copy_psa_key psa_key =
      let pk = Mbedtls.pk_init () in
      let st = Mbedtls.pk_copy_from_psa psa_key pk in
      if Mbedtls.Status.is_ok st then Ok pk else
      Error (msg_of_status "copy-psa-key" st)

    let generate () =
      let op = "key-gen" in
      let pk = Mbedtls.pk_init () in
      let attrs = Psa.Key_attributes.init () in
      let alg = Psa.Alg.ecdsa_any in
      let key_type = Psa.Key_type.ecc_key_pair Psa.Ecc_family.secp_r1 in
      let key_bits = 256 in (* secp256r1 *)
      let () = Psa.set_key_usage_flags attrs Psa.Key_usage.export in
      let () = Psa.set_key_algorithm attrs alg in
      let () = Psa.set_key_type attrs key_type in
      let () = Psa.set_key_bits attrs key_bits in
      match Psa.generate_key attrs with
      | Error e -> Error (msg_of_psa_status op e)
      | Ok kid ->
          let st = Mbedtls.pk_copy_from_psa kid pk in
          ignore (Psa.destroy_key kid);
          if Mbedtls.Status.is_ok st then Ok pk else Error (msg_of_status op st)
  end

  type own = t * Private_key.t
  type ptime = float

  let ptime_now () = Unix.gettimeofday ()
  let d47_later t = t +. (86_400. *. 47.)
  let utc_datetime_of_ptime t =
    let utc : Unix.tm = Unix.gmtime t in
    Printf.sprintf "%04d%02d%02d%02d%02d%02d"
      (utc.tm_year + 1900) (utc.tm_mon + 1) utc.tm_mday
      utc.tm_hour utc.tm_min utc.tm_sec

  let self_signed
      ?private_key ?invalid_before ?invalid_after ?(is_ca = false)
      ?(name = "localhost") ()
    =
    let op = "self-cert-gen" in
    let invalid_before = match invalid_before with
    | Some t -> t | None -> Unix.gettimeofday ()
    in
    let invalid_after = match invalid_after with
    | Some t -> t | None -> d47_later invalid_before
    in
    let invalid_before = utc_datetime_of_ptime invalid_before in
    let invalid_after = utc_datetime_of_ptime invalid_after in
    let cert = Mbedtls.x509_crt_init () in
    let private_key = match private_key with
    | Some private_key -> Ok private_key | None -> Private_key.generate ()
    in
    match private_key with
    | Error e -> Error (Printf.sprintf "%s: %s" op e)
    | Ok issuer_key ->
        let issuer_name = "CN=" ^ name in
        let subject_alt_dns = if is_ca then "" else name in
        let subject_name = issuer_name and subject_key = issuer_key in
        let st =
          Mbedtls.x509_crt_generate
            cert ~invalid_before ~invalid_after ~is_ca ~issuer_name
            ~issuer_key ~subject_name ~subject_key ~subject_alt_dns
        in
        if Mbedtls.Status.is_ok st then Ok (cert, issuer_key) else
        Error (msg_of_status op st)

  let ca_signed
      ~ca:(ca_cert, issuer_key) ?private_key ?invalid_before ?invalid_after
      ?(name = "localhost") ()
    =
    let op = "ca-signed-cert" in
    let invalid_before = match invalid_before with
    | Some t -> t | None -> Unix.gettimeofday ()
    in
    let invalid_after = match invalid_after with
    | Some t -> t | None -> d47_later invalid_before
    in
    let invalid_before = utc_datetime_of_ptime invalid_before in
    let invalid_after = utc_datetime_of_ptime invalid_after in
    let cert = Mbedtls.x509_crt_init () in
    let private_key = match private_key with
    | Some private_key -> Ok private_key | None -> Private_key.generate ()
    in
    match private_key with
    | Error e -> Error (Printf.sprintf "%s: %s" op e)
    | Ok subject_key ->
        let issuer_name = Mbedtls.x509_crt_get_subject_name ca_cert in
        let subject_alt_dns = name in
        let subject_name = "CN=" ^ name in
        let st =
          Mbedtls.x509_crt_generate
            cert ~invalid_before ~invalid_after ~is_ca:false ~issuer_name
            ~issuer_key ~subject_name ~subject_key ~subject_alt_dns
        in
        if not (Mbedtls.Status.is_ok st) then Error (msg_of_status op st) else
        (* Add the CA to the chain *)
        let add_der_cert cert der = internal_append_der_cert cert der; cert in
        let cert = fold_der_certs add_der_cert cert ca_cert in
        try Ok (cert, subject_key) with
        | Failure msg -> Error (Printf.sprintf "%s: %s" op msg)
end

(* TLS configuration *)

module Conf = struct
  type tls_version = Mbedtls.ssl_protocol_version = Tls_v1_2 | Tls_v1_3
  let pp_tls_version ppf = function
  | Tls_v1_2 -> Format.pp_print_string ppf "1.2"
  | Tls_v1_3 -> Format.pp_print_string ppf "1.3"

  type kind = Client | Server
  let pp_kind ppf = function
  | Client -> Format.pp_print_string ppf "client"
  | Server -> Format.pp_print_string ppf "server"

  type t =
    { kind : kind;
      alpn_protocols : string list;
      min_tls_version : tls_version;
      max_tls_version : tls_version;
      own_certs : X509_certchain.own list;
      trusted_certs : X509_certchain.t list;
      trusted_certs_bundle :
        (* This is [trusted_certs] as a single chain, the api requires that *)
        X509_certchain.t option;
      verify_peer : bool;
      (* Note once we associate a conf to a context these two field
         below  must be kept live until the context is disposed. *)
      alpn_protocols_for_c : Mbedtls.alpn_protocols;
      ssl_conf : Mbedtls.ssl_config; }

  exception Setup_error of string

  let is_ok st =
    if Mbedtls.Status.is_ok st then () else
    raise (Setup_error (msg_of_status "setup" st))

  let setup conf =
    let set_own_cert ssl_conf (chain, k) =
      is_ok @@ Mbedtls.ssl_conf_own_cert ssl_conf chain k
    in
    let endpoint = match conf.kind with
    | Client -> Mbedtls.SSL_IS_CLIENT
    | Server -> Mbedtls.SSL_IS_SERVER
    in
    let transport = Mbedtls.SSL_TRANSPORT_STREAM in
    let preset = Mbedtls.SSL_PRESET_DEFAULT in
    let authmode : Mbedtls.enum =
      if conf.verify_peer then SSL_VERIFY_REQUIRED else SSL_VERIFY_NONE
    in
    let ssl_conf = conf.ssl_conf in
    is_ok @@ Mbedtls.ssl_config_defaults ssl_conf endpoint transport preset;
    is_ok @@ (* See https://github.com/Mbed-TLS/mbedtls/issues/10461 *)
    (if conf.alpn_protocols = [] then Mbedtls.Status.ok else
     Mbedtls.ssl_conf_alpn_protocols ssl_conf conf.alpn_protocols_for_c);
    begin match conf.trusted_certs_bundle with
    | None -> ()
    | Some trusted_certs -> Mbedtls.ssl_conf_ca_chain ssl_conf trusted_certs
    end;
    List.iter (set_own_cert ssl_conf) conf.own_certs;
    Mbedtls.ssl_conf_authmode ssl_conf authmode;
    Mbedtls.ssl_conf_min_tls_version ssl_conf conf.min_tls_version;
    Mbedtls.ssl_conf_max_tls_version ssl_conf conf.max_tls_version;
    ()

  let make
      ?(alpn_protocols = []) ?(min_tls_version = Tls_v1_2)
      ?(max_tls_version = Tls_v1_3) ?(own_certs = []) ?trusted_certs
      ?verify_peer kind
    =
    let verify_peer = match verify_peer with
    | Some verify -> verify
    | None -> match kind with Client -> true | Server -> false
    in
    let* trusted_certs = match trusted_certs with
    | Some cs -> Ok cs
    | None ->
        match kind with
        | Client when verify_peer ->
            begin match X509_certchain.system_ca_certs () with
            | Ok None -> Error "No system trusted CA certificates found"
            | Ok Some c -> Ok [c]
            | Error _ as e -> e
            end
        | _ -> Ok []
    in
    let trusted_certs_bundle = match trusted_certs with
    | [] -> None | css -> Some (X509_certchain.concat css)
    in
    let* own_certs = match kind with
    | Client -> (* XXX error if multiple ? *) Ok own_certs
    | Server ->
        begin match own_certs with
        | [] -> Error "No own certificate provided for server configuration"
        | own_certs -> Ok own_certs
        end
    in
    let alpn_protocols_for_c =
      Mbedtls.alpn_protocols_init (Array.of_list alpn_protocols)
    in
    let ssl_conf = Mbedtls.ssl_config_init () in
    let conf =
      { kind; alpn_protocols; alpn_protocols_for_c; min_tls_version;
        max_tls_version; own_certs; trusted_certs; trusted_certs_bundle;
        verify_peer; ssl_conf }
    in
    try setup conf; Ok conf with Setup_error e -> Error e

  let alpn_protocols c = c.alpn_protocols
  let kind c = c.kind
  let min_tls_version c = c.min_tls_version
  let max_tls_version c = c.max_tls_version
  let trusted_certs c = c.trusted_certs
  let own_certs c = c.own_certs
  let verify_peer c = c.verify_peer
  let ssl_conf c = c.ssl_conf
  let keep_until_here c = ignore (Sys.opaque_identity c)
end

(* TLS connection information *)

module Info = struct
  type t =
    { ssl_ctx : Mbedtls.ssl_context;
      mutable is_valid : bool;
      mutable ctx_closed : bool; }

  let make ssl_ctx = { ssl_ctx; is_valid = true; ctx_closed = false; }
  let is_valid i = i.is_valid
  let ssl_ctx i = i.ssl_ctx
  let invalidate i = i.is_valid <- false
  let close_ctx i =
    if i.ctx_closed then Mbedtls.Status.ok else
    (i.ctx_closed <- true; Mbedtls.ssl_close_notify i.ssl_ctx)

  let[@inline] check_valid i =
    if not i.is_valid
    then failwith "Trying to get info from closed TLS stream"

  let tls_version i = check_valid i; Mbedtls.ssl_get_version_number i.ssl_ctx
  let alpn_protocol i = check_valid i; Mbedtls.ssl_get_alpn_protocol i.ssl_ctx
  let peer_cert i = check_valid i; Mbedtls.ssl_get_peer_cert i.ssl_ctx

  let pp ppf i =
    let pp_tls_version ppf = function
    | None -> Format.pp_print_string ppf "<unknown>"
    | Some v -> Conf.pp_tls_version ppf v
    in
    let pp_alpn_protocol ppf = function
    | None -> Format.pp_print_string ppf "<none>"
    | Some p -> Format.pp_print_string ppf p
    in
    let pp_peer_cert ppf = function
    | None -> Format.pp_print_string ppf "no"
    | Some _ -> Format.pp_print_string ppf "yes"
    in
    Format.fprintf ppf "@[TLS %a alpn: %a peer-cert: %a@]"
      pp_tls_version (tls_version i) pp_alpn_protocol (alpn_protocol i)
      pp_peer_cert (peer_cert i)

  let pp_peer pp_cert ppf i = match peer_cert i with
  | None -> Format.pp_print_string ppf "No peer certificate"
  | Some cs -> Format.fprintf ppf "@[<v>Peer certificate:@,%a@]" pp_cert cs

  let pp_peer_cert ppf i = pp_peer X509_certchain.pp ppf i
  let pp_peer_cert_pem ppf i = pp_peer X509_certchain.pp_pem ppf i
end

let backend_info () =
  let (maj, min, bug)  = Mbedtls.version () in
  Printf.sprintf "Mbed TLS %d.%d.%d" maj min bug

(* Stream errors *)

type Bytes.Stream.error += Error of string

let format_error =
  let case msg = Error msg in
  let message = function Error msg -> msg | _ -> assert false in
  Bytes.Stream.make_format_error ~format:"tls" ~case ~message

let stream_error e = Bytes.Stream.error format_error e

(* Streams, note this assume a blocking fd *)

let bytes_reader_of_info ?(slice_length = Bytes.Slice.unix_io_buffer_size) info
  =
  let b = Bytes.create (Bytes.Slice.check_length slice_length) in
  let rec read () =
    if not (Info.is_valid info)
    then invalid_arg "Trying to read from closed TLS stream" else
    match Mbedtls.ssl_read (Info.ssl_ctx info) b 0 slice_length with
    | 0 -> Bytes.Slice.eod
    | count when count > 0 -> Bytes.Slice.make b ~first:0 ~length:count
    | st -> stream_error (Mbedtls.Status.message st)
  in
  Bytes.Reader.make ~slice_length read

let bytes_writer_of_info
    ?pos ?(slice_length = Bytes.Slice.unix_io_buffer_size) fd info
  =
  let rec write s =
    if not (Info.is_valid info)
    then invalid_arg "Trying to write to a closed TLS stream" else
    match s with
    | s when Bytes.Slice.is_eod s ->
        let st = Info.close_ctx info in
        if not (Mbedtls.Status.is_ok st)
        then stream_error (Mbedtls.Status.message st)
    | s ->
        let b = Bytes.Slice.bytes s in
        let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
        match Mbedtls.ssl_write (Info.ssl_ctx info) b first length with
        | count when count = length -> ()
        | count when count >= 0 -> write (Option.get (Bytes.Slice.drop count s))
        | st -> stream_error (Mbedtls.Status.message st)
  in
  Bytes.Writer.make ~slice_length write

(* Socket setup *)

let for_client_socket conf ~peer_hostname ~peer:fd f =
  if Conf.kind conf <> Client then invalid_arg "Not a client configuration" else
  let ssl_ctx = Mbedtls.ssl_context_init () in
  let info = Info.make ssl_ctx in
  let finally () =
    Info.invalidate info;
    Mbedtls.ssl_context_destroy ssl_ctx;
    Conf.keep_until_here conf; (* For alpn_protocols_c and ssl_conf *)
  in
  Fun.protect ~finally @@ fun () ->
  try
    Conf.is_ok @@ Mbedtls.ssl_setup ssl_ctx (Conf.ssl_conf conf);
    Mbedtls.ssl_set_bio_to_socket_fd ssl_ctx fd;
    Conf.is_ok @@ Mbedtls.ssl_set_hostname ssl_ctx peer_hostname;
    Conf.is_ok @@ Mbedtls.ssl_handshake ssl_ctx;
    let finally () = ignore (Info.close_ctx info) in
    Fun.protect ~finally @@ fun () ->
    let send = bytes_writer_of_info fd info in
    let recv = bytes_reader_of_info info in
    Ok (f info ~peer:(send, recv))
  with Conf.Setup_error msg -> Error msg

let for_server_socket conf ~peer:fd f =
  if Conf.kind conf <> Server then invalid_arg "Not a server configuration" else
  let ssl_ctx = Mbedtls.ssl_context_init () in
  let info = Info.make ssl_ctx in
  let finally () =
    Info.invalidate info;
    Mbedtls.ssl_context_destroy ssl_ctx;
    Conf.keep_until_here conf; (* For alpn_protocols_c and ssl_conf *)
  in
  Fun.protect ~finally @@ fun () ->
  try
    Conf.is_ok @@ Mbedtls.ssl_setup ssl_ctx (Conf.ssl_conf conf);
    Mbedtls.ssl_set_bio_to_socket_fd ssl_ctx fd;
    Conf.is_ok @@ Mbedtls.ssl_handshake ssl_ctx;
    let finally () = ignore (Info.close_ctx info) in
    Fun.protect ~finally @@ fun () ->
    let send = bytes_writer_of_info fd info in
    let recv = bytes_reader_of_info info in
    Ok (f info ~peer:(send, recv))
  with Conf.Setup_error msg -> Stdlib.Error msg
