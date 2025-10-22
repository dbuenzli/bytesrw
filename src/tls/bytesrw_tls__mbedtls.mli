(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Thin bindings to Mbedtls API.

    {b Warning.} This is an internal API not for the casual user. The
    binding does not always protect against use after free errors.
    The API may change between minor versions of the library.

    {b References.}
    {ul
    {- {{:https://mbed-tls.readthedocs.io/projects/api/en/development/}
       Mbed TLS API documentation}}} *)

open Bytesrw

val version : unit -> int * int * int
(** [version ()] is the mbedtls [maj.min.patch] version number. *)

val base64_encode : string -> string
(** [base64_encode s] is the base64 encoding of [s]. *)

(** {1:status Status codes} *)

(** Status codes.

    Note that mbedtls has no proper type for that it uses C [int].  *)
module Status : sig
  type t = int

  val ok : t
  (** [ok] is [0]. *)

  val is_ok : t -> bool
  (** [is_ok st]  is [equal st ok]. *)

  val equal : t -> t -> bool
  (** [equal] tests status codes for equality. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  val message : t -> string
  (** [message c] is a message for status code [s]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats status codes for inspection. *)
end

(** {1:enum Enum constants} *)

type ssl_protocol_version =
| Tls_v1_2
| Tls_v1_3 (** *)
(** The type for a selection of cases for the [mbedtls_ssl_protocol_version]
    enum. *)

type enum =
| SSL_TRANSPORT_STREAM
| SSL_TRANSPORT_DATAGRAM
| SSL_IS_CLIENT
| SSL_IS_SERVER
| SSL_VERIFY_NONE
| SSL_VERIFY_OPTIONAL
| SSL_VERIFY_REQUIRED
| SSL_PRESET_DEFAULT
(** The type for a selection of (#ifdef'd) mbedtls enum constants. *)

(** {1:pk Private keys} *)

type pk_context
(** The type for privates keys. *)

val pk_init : unit -> pk_context
(** [pk_init ()] create and initializes a private key context. The value
    is finalized. *)

val pk_parse_keyfile : pk_context -> string -> Status.t
(** Binds [mbedtls_pk_parse_keyfile]. *)

val pk_write_keyfile : pk_context -> string -> Status.t
(** [pk_writes_keyfile pk file] writes [pk] to [file]. May raise [Failure _]. *)

val pk_copy_from_psa : Bytesrw_crypto.Psa.Key_id.t -> pk_context -> Status.t
(** Binds [mbedtls_pk_copy_from_psa]. *)

(** {1:x509 X.509 certificates} *)

type x509_crt
(** The type for [mbedtls_x509_crt]. Finalized. *)

val x509_crt_init : unit -> x509_crt
(** [x509_crt_init ()] creates and initializes an X.509 certificate.
    The value is finalized. *)

val x509_crt_parse : x509_crt -> string -> Status.t
(** Binds [mbedtls_x509_crt_parse]. *)

val x509_crt_parse_der : x509_crt -> string -> Status.t
(** Binds [mbedtls_x509_crt_parse_der]. *)

val x509_crt_get_issuer_name : x509_crt -> string
(** Issuer name of last certificate in the chain. *)

val x509_crt_get_subject_name : x509_crt -> string
(** Subject name of last certificate in the chain. *)

val x509_crt_get_valid_from : x509_crt -> (int * int * int * int * int * int)
(** Start of validitiy of last certificate in the chain. *)

val x509_crt_get_valid_to : x509_crt -> (int * int * int * int * int * int)
(** End of validitiy of last certificate in the chain. *)

val x509_crt_get_serial : x509_crt -> string
(** Serial number of last certificate in the chain. *)

val x509_crt_get_is_ca : x509_crt -> bool
(** Indicates if the certificate is a certificate authority. *)

val x509_crt_of_system_store : unit -> x509_crt option
(** Looks up certificates in the system store. Currently only returns
    something on Windows and Darwin. Raises [Failure _] in case of error.*)

val x509_crt_iter_raw_der_chain : x509_crt -> (string -> unit) -> unit
(** [x509_crt_iter_raw_der_chain c f] iters over the raw certificate
    data in DER format of the chain [c] via [f]. *)

val x509_crt_generate :
  x509_crt -> invalid_before:string -> invalid_after:string ->
  is_ca:bool -> issuer_name:string -> issuer_key:pk_context ->
  subject_name:string -> subject_key:pk_context ->
  subject_alt_dns:string -> Status.t
(** [x509_crt_generate c pk …] generates in [c] a certificate. *)

(** {1:config SSL config} *)

type alpn_protocols
(** The type for list of alpn protocols. Finalized. *)

val alpn_protocols_init : string array -> alpn_protocols
(** [alpn_protocols_init ps] create and initializes a list of protocols. *)

type ssl_config
(** The type for [mbedtls_ssl_config]. Finalized. *)

val ssl_config_init : unit -> ssl_config
(** [ssl_config_init ()] creates and initializes an SSL config
    with [mbedtls_ssl_config_init]. This
    value must be eventually disposed with {!ssl_config_destroy}. *)

val ssl_config_defaults : ssl_config -> enum -> enum -> enum -> Status.t
(** Binds [mbedtls_ssl_config_defaults]. *)

val ssl_conf_authmode : ssl_config -> enum -> unit
(** Binds [mbedtls_ssl_conf_authmode]. *)

val ssl_conf_ca_chain : ssl_config -> x509_crt -> unit
(** Binds [mbedtls_ssl_conf_ca_chain] (revocation list is set to NULL). *)

val ssl_conf_own_cert : ssl_config -> x509_crt -> pk_context -> Status.t
(** Binds [mbedtls_ssl_conf_own_cert]. *)

val ssl_conf_min_tls_version : ssl_config -> ssl_protocol_version -> unit
(** Binds [mbedtls_ssl_conf_min_tls_version]. *)

val ssl_conf_max_tls_version : ssl_config -> ssl_protocol_version -> unit
(** Binds [mbedtls_ssl_conf_max_tls_version]. *)

val ssl_conf_alpn_protocols : ssl_config -> alpn_protocols -> Status.t
(** Binds [mbedtsl_ssl_conf_alpn_protocols]. Warning [alpn_protocols]
    must be kept alive until the context is. *)

(** {1:ctx SSL context} *)

type ssl_context
(** The type for [mbedtls_ssl_context]. Not finalized. *)

val ssl_context_init : unit -> ssl_context
(** [ssl_context_init] creates and initializes an SSL context with
    [mbedtls_ssl_init]. This value must be eventually disposed with
    {!ssl_destroy}. *)

val ssl_context_destroy : ssl_context -> unit
(** [ssl_context_destory ctx] destroys context [ctx]. *)

val ssl_setup : ssl_context -> ssl_config -> Status.t
(** Binds [mbedtls_ssl_setup]. *)

val ssl_set_hostname : ssl_context -> string -> Status.t
(** Binds [mbedtls_ssl_set_hostname].

    @raise Invalid_argument if the hostname contains stray NULL bytes. *)

val ssl_set_bio_to_socket_fd : ssl_context -> Unix.file_descr -> unit
(** [ssl_set_bio_to_socket_fd ctx fd] calls [mbedtls_ssl_set_fd] that
    [send] and [recv] according to whether [fd] is blocking or not. *)

val ssl_handshake : ssl_context -> Status.t
(** Binds [mbedtls_ssl_handshake] and [mbedtls_ssl_handshake_step]. *)

val ssl_get_version_number : ssl_context -> ssl_protocol_version option
(** Binds [mbedtls_ssl_get_version_number]. *)

val ssl_get_alpn_protocol : ssl_context -> string option
(** Binds [mbedtls_ssl_get_alpn_protocol]. *)

val ssl_get_peer_cert : ssl_context -> x509_crt option
(** Binds [mbedtls_ssl_get_peer_cert]. *)

val ssl_close_notify : ssl_context -> Status.t
(** Binds [mbedtls_ssl_close_notify]. *)

val ssl_read : ssl_context -> bytes -> int -> int -> int
(** [ssl_read ctx b first len] reads [len] bytes from the connection
    and stores them in [b] starting at [first]. Returns the amount of
    read bytes or a {!Status.t} if the value is negative.

    [0] is returned if [mbedtls_ssl_read] returned [0] or if
    it returned [MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY]. This means
    that no data will be sent anymore. *)

val ssl_write : ssl_context -> bytes -> int -> int -> int
(** [ssl_write ctx b first len] writes [len] bytes from [b] starting
    [first] to the connection. Returns the amount of written bytes or
    a {!Status.t} if the value is negative. *)
