(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let invalid_range f ~first ~length ~len =
  Printf.ksprintf invalid_arg "%s: invalid range: first:%d length:%d bytes:%d"
    f first length len

(* Library management *)

external version : unit -> int * int * int =
  "ocaml_bytesrw_mbedtls_version"

external base64_encode : string -> string =
  "ocaml_bytesrw_mbedtls_base64_encode"

(* Status codes *)

module Status = struct
  type t = int
  let ok = 0
  let equal = Int.equal
  let is_ok st = equal ok st
  let compare = Int.compare

  external message : t -> string =
    "ocaml_bytesrw_mbedtls_strerror"

  let pp ppf st = Format.pp_print_string ppf (message st)
end

type ssl_protocol_version =
(* keep in sync with ocaml_mbedtls_ssl_protocol_version and
   ocaml_bytesrw_mbedtls_ssl_get_version in C stub *)
| Tls_v1_2
| Tls_v1_3

type enum =
(* keep in sync with ocaml_mbedtls_enum table in C stub *)
| SSL_TRANSPORT_STREAM
| SSL_TRANSPORT_DATAGRAM
| SSL_IS_CLIENT
| SSL_IS_SERVER
| SSL_VERIFY_NONE
| SSL_VERIFY_OPTIONAL
| SSL_VERIFY_REQUIRED
| SSL_PRESET_DEFAULT

(* Private keys *)

type pk_context
(* Custom value with a pointer to an mbedtls_pk_context. Finalized
   but can be manually disposed. *)

external pk_init : unit -> pk_context =
  "ocaml_bytesrw_mbedtls_pk_init"

external pk_parse_keyfile : pk_context -> string -> Status.t =
  "ocaml_bytesrw_mbedtls_pk_parse_keyfile"

external pk_write_keyfile : pk_context -> string -> Status.t =
  "ocaml_bytesrw_mbedtls_pk_write_keyfile"

external pk_destroy : pk_context -> unit =
  "ocaml_bytesrw_mbedtls_pk_destroy"

external pk_is_valid : pk_context -> bool =
  "ocaml_bytesrw_mbedtls_pk_is_valid"

external pk_copy_from_psa :
  Bytesrw_crypto.Psa.Key_id.t -> pk_context -> Status.t =
  "ocaml_bytesrw_mbedtls_pk_copy_from_psa"

(* X.509 certificates *)

type x509_crt
(* Custom value with a pointer to an mbedtls_x509_crt. Finalized. *)

external x509_crt_init : unit -> x509_crt =
  "ocaml_bytesrw_mbedtls_x509_crt_init"

external x509_crt_parse : x509_crt -> string -> Status.t =
  "ocaml_bytesrw_mbedtls_x509_crt_parse"

external x509_crt_parse_der : x509_crt -> string -> Status.t =
  "ocaml_bytesrw_mbedtls_x509_crt_parse_der"

external unsafe_x509_crt_get_issuer_name : x509_crt -> bytes -> Status.t =
  "ocaml_bytesrw_x509_crt_get_issuer_name"

external unsafe_x509_crt_get_subject_name : x509_crt -> bytes -> Status.t =
  "ocaml_bytesrw_x509_crt_get_subject_name"

external x509_crt_get_valid_from :
  x509_crt -> (int * int * int * int * int * int) =
  "ocaml_bytesrw_x509_crt_get_valid_from"

external x509_crt_get_valid_to :
  x509_crt -> (int * int * int * int * int * int) =
  "ocaml_bytesrw_x509_crt_get_valid_to"

external x509_crt_get_serial : x509_crt -> string =
  "ocaml_bytesrw_x509_crt_get_serial"

external x509_crt_get_is_ca : x509_crt -> bool =
  "ocaml_bytesrw_x509_crt_get_is_ca"

let get unsafe c =
  let b = Bytes.create 1024 in
  let len = unsafe c b in
  if len < 0 then "Error: " ^ Status.message len else
  Bytes.sub_string b 0 len

let x509_crt_get_issuer_name = get unsafe_x509_crt_get_issuer_name
let x509_crt_get_subject_name = get unsafe_x509_crt_get_subject_name

external x509_crt_of_system_store : unit -> x509_crt option =
  "ocaml_bytesrw_x509_crt_of_system_store"

external x509_crt_iter_raw_der_chain : x509_crt -> (string -> unit) -> unit =
  "ocaml_bytesrw_x509_crt_iter_raw_der_chain"

external x509_crt_generate :
  x509_crt -> invalid_before:string -> invalid_after:string ->
  is_ca:bool -> issuer_name:string -> issuer_key:pk_context ->
  subject_name:string -> subject_key:pk_context -> subject_alt_dns:string ->
  Status.t
  =
  "ocaml_bytesrw_x509_crt_generate_bc" "ocaml_bytesrw_x509_crt_generate"

(* Config *)

type alpn_protocols
(* Custom value with a pointer on a NULL terminated array of C strings.
   Finalized. Note that once we give it to a config we need to manage
   its life-time because ssl_config makes no copy. *)

external alpn_protocols_init : string array -> alpn_protocols =
  "ocaml_bytesrw_alpn_protocols_init"

type ssl_config
(* Custom value with a pointer to a mbedtsl_ssl_config. Finalized. *)

external ssl_config_init : unit -> ssl_config =
  "ocaml_bytesrw_mbedtls_ssl_config_init"

external ssl_config_defaults : ssl_config -> enum -> enum -> enum -> Status.t =
  "ocaml_bytesrw_mbedtls_ssl_config_defaults"

external ssl_conf_authmode : ssl_config -> enum -> unit =
  "ocaml_bytesrw_mbedtls_ssl_conf_authmode"

external ssl_conf_ca_chain : ssl_config -> x509_crt -> unit =
  "ocaml_bytesrw_mbedtls_ssl_conf_ca_chain"

external ssl_conf_own_cert : ssl_config -> x509_crt -> pk_context -> Status.t =
  "ocaml_bytesrw_mbedtls_ssl_conf_own_cert"

external ssl_conf_min_tls_version : ssl_config -> ssl_protocol_version -> unit =
  "ocaml_bytesrw_mbedtls_ssl_conf_min_tls_version"

external ssl_conf_max_tls_version : ssl_config -> ssl_protocol_version -> unit =
  "ocaml_bytesrw_mbedtls_ssl_conf_max_tls_version"

external ssl_conf_alpn_protocols : ssl_config -> alpn_protocols -> Status.t =
  "ocaml_byterw_mbedtls_ssl_conf_alpn_protocols"

(* Context *)

type ssl_context
(* Custom value with a pointer to a mbedtsl_ssl_context. Not finalized. *)

external ssl_context_init : unit -> ssl_context =
  "ocaml_bytesrw_mbedtls_ssl_context_init"

external ssl_context_destroy : ssl_context -> unit =
  "ocaml_bytesrw_mbedtls_ssl_context_destroy"

external ssl_setup : ssl_context -> ssl_config -> Status.t =
  "ocaml_bytesrw_mbedtls_ssl_setup"

external ssl_set_hostname : ssl_context -> string -> Status.t =
  "ocaml_bytesrw_mbedtls_ssl_set_hostname"

external ssl_set_bio_to_socket_fd : ssl_context -> Unix.file_descr -> unit =
  "ocaml_bytesrw_mbedtls_ssl_set_bio_to_socket_fd"

external ssl_handshake : ssl_context -> Status.t =
  "ocaml_bytesrw_mbedtls_ssl_handshake"

external ssl_get_version_number : ssl_context -> ssl_protocol_version option =
  "ocaml_bytesrw_mbedtls_ssl_get_version_number"

external ssl_get_alpn_protocol : ssl_context -> string option =
  "ocaml_bytesrw_mbedtls_ssl_get_alpn_protocol"

external ssl_get_peer_cert : ssl_context -> x509_crt option =
  "ocaml_bytesrw_mbedtls_ssl_get_peer_cert"

external ssl_close_notify : ssl_context -> Status.t =
  "ocaml_bytesrw_mbedtls_ssl_close_notify"

external unsafe_ssl_read : ssl_context -> bytes -> int -> int -> int =
  "ocaml_bytesrw_mbedtls_ssl_read"

external unsafe_ssl_write : ssl_context -> bytes -> int -> int -> int =
  "ocaml_bytesrw_mbedtls_ssl_write"

let ssl_read fd b first length =
  let len = Bytes.length b in
  if first < 0 || length < 0 || first + length > len
  then invalid_range "Mbedtls.ssl_read" ~first ~length ~len
  else unsafe_ssl_read fd b first len

let ssl_write fd b first length =
  let len = Bytes.length b in
  if first < 0 || length < 0 || first + length > len
  then invalid_range "Mbedtls.ssl_write" ~first ~length ~len
  else unsafe_ssl_write fd b first len
