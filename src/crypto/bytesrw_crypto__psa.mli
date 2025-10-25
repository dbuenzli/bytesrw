(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Thin bindings to PSA Crypto API.

    The binding is low-level and mostly one-to-one with C. It does not
    try to embelish the API for OCaml idioms. However given a
    memory-safe implementation of PSA Crypto, this binding allows no
    unsafe usage.

    {b Key material.} When key material is manipulated explicitly
    ({!Psa.import_key}, {!Psa.export_key}, etc.) we use
    {!Psa.bigbytes} to store the bytes. Since a bigbytes value wraps a
    C [malloc]ed pointer, it prevents the garbage collector from
    copying the key material and {!Bytesrw_crypto.Clear}ing it
    destroys it. Of course if you use {!Bigbytes.of_string} or
    {!Bigbytes.of_bytes} to create the bigbytes value that defeats the
    purpose. Note that OCaml 5.2 added functions to directly read and
    write bigbytes values with channels and file descriptors.

    {b Naming convention}. Any C identifier of the form
    [psa_xxx_yyy_zzz] or [PSA_XXX_YYY_ZZZ] is mapped in OCaml to on
    one of [Psa.xxx_yyy_zzz], [Psa.Xxx.yyy_zzz].  There may be a few
    exception to the rule (e.g.  {!Psa.Cipher.block_length}).

    {b Return value convention.} When C functions return by setting a
    value passed by reference (e.g. a key identifier or number of
    bytes written) if and only the call is a {!success} we return that
    value in an OCaml [Ok] value and otherwise [Error] value with the
    error {{!Status.t}status code}. See for example
    {!Psa.generate_key}, {!Psa.export_key}, etc.

    {b Supported version and references.} For now we bind the version 1.2 of
    the API with what is available in the
    {{:https://github.com/Mbed-TLS/TF-PSA-Crypto}TF-PSA-Crypto}
    implementation of the specification (a few entry points are missing).
    {ul
    {- {{:https://arm-software.github.io/psa-api/crypto/1.2/}
       PSA Certified Crypto API 1.2}}}

    {b Sample code.} The
    {{:https://github.com/dbuenzli/bytesrw/blob/main/test/test_psa.ml}test
    suite} can double up as (not the best) sample code. *)

open Bytesrw

(** {1:preliminaries Preliminaries}

    {b Note.} These are OCaml artefacts, not part of the PSA Crypto
    API. *)

type uint8 = int
(** The type for unsigned 8-bit integers. *)

type uint16 = int
(** The type for unsigned 16-bit integers. *)

type uint32 = int32
(** The type for unsigned 32-bit integers. *)

type uint64 = int64
(** The type for unsigned 64-bit integers. *)

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type for bigarrays of bytes. See also {!Bytesrw_crypto.Bigbytes}. *)

(** {1:status_codes {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#status-codes}Status codes}} *)

(** Status codes. *)
module Status : sig
  type t = int
  (** The type for {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.psa_status_t}[psa_status_t]}. *)

  val equal : t -> t -> bool
  (** [equal] tests status codes for equality. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  val message : t -> string
  (** [message c] is a message for status code [s]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats status codes for inspection. *)

  (** Binding operators.

      This allows to inject bare status function results into
      the [('a, t) result] error monad. It binds on
      {!Bytesrw_crypto__psa.success} and otherwise [Error ] with
      the error code. *)
  module Syntax : sig
    val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
    (** This is the usual {!Result.bind} *)

    val ( let+ ) : t -> (unit -> ('b, t) result) -> ('b, t) result
    (** This binds on {!Bytesrw_crypto__psa.success} and otherwise
        [Error _] with the error code. *)
  end

end

val success : Status.t
(** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_SUCCESS}[PSA_SUCCESS]} *)

val operation_incomplete : Status.t
(** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_OPERATION_INCOMPLETE}[PSA_OPERATION_INCOMPLETE]} *)

(** Error statuses. *)
module Error : sig
  val programmer_error : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_PROGRAMMER_ERROR}[PSA_ERROR_PROGRAMMER_ERROR]} *)

  val connection_refused : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_CONNECTION_REFUSED}[PSA_ERROR_CONNECTION_REFUSED]} *)

  val connection_busy : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_CONNECTION_BUSY}[PSA_ERROR_CONNECTION_BUSY]} *)

  val generic_error : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_GENERIC_ERROR}[PSA_ERROR_GENERIC_ERROR]} *)

  val not_permitted : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codesx.html#c.PSA_ERROR_NOT_PERMITTED}[PSA_ERROR_NOT_PERMITTED]} *)

  val not_supported : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_NOT_SUPPORTED}[PSA_ERROR_NOT_SUPPORTED]} *)

  val invalid_argument : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_INVALID_ARGUMENT}[PSA_ERROR_INVALID_ARGUMENT]} *)

  val invalid_handle : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_INVALID_HANDLE}[PSA_ERROR_INVALID_HANDLE]} *)

  val bad_state : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_BAD_STATE}[PSA_ERROR_BAD_STATE]} *)

  val buffer_too_small : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_buffer_too_small}[PSA_ERROR_buffer_too_small]} *)

  val already_exists : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_ALREADY_EXISTS}[PSA_ERROR_ALREADY_EXISTS]} *)

  val does_not_exist : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_DOES_NOT_EXIST}[PSA_ERROR_DOES_NOT_EXIST]} *)

  val insufficient_memory : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_INSUFFICIENT_MEMORY}[PSA_ERROR_INSUFFICIENT_MEMORY]} *)

  val insufficient_storage : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_INSUFFICIENT_STORAGE}[PSA_ERROR_INSUFFICIENT_STORAGE]} *)

  val insufficient_data : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_INSUFFICIENT_DATA}[PSA_ERROR_INSUFFICIENT_DATA]} *)

  val service_failure : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_SERVICE_FAILURE}[PSA_ERROR_SERVICE_FAILURE]} *)

  val communication_failure : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_COMMUNICATION_FAILURE}[PSA_ERROR_COMMUNICATION_FAILURE]} *)

  val storage_failure : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_STORAGE_FAILURE}[PSA_ERROR_STORAGE_FAILURE]} *)

  val hardware_failure : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_HARDWARE_FAILURE}[PSA_ERROR_HARDWARE_FAILURE]} *)

  val insufficient_entropy : Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/library/status.html#c.PSA_ERROR_INSUFFICIENT_ENTROPY}[PSA_ERROR_INSUFFICIENT_ENTROPY]} *)

  val invalid_signature : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_INVALID_SIGNATURE}[PSA_ERROR_INVALID_SIGNATURE]} *)

  val invalid_padding : Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/library/status.html#c.PSA_ERROR_INVALID_PADDING}[PSA_ERROR_INVALID_PADDING]} *)

  val corruption_detected : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_CORRUPTION_DETECTED}[PSA_ERROR_CORRUPTION_DETECTED]} *)

  val data_corrupt : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_DATA_CORRUPT}[PSA_ERROR_DATA_CORRUPT]} *)

  val data_invalid : Status.t
  (** {{:https://arm-software.github.io/psa-api/status-code/1.0/api/status-codes.html#c.PSA_ERROR_DATA_INVALID}[PSA_ERROR_DATA_INVALID]} *)
end

(** {1:library_management
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/library/index.html}
    Library management}} *)

val crypto_api_version : unit -> (int * int)
(** [crypto_api_version ()] is the {{:https://arm-software.github.io/psa-api/crypto/1.2/api/library/library.html#api-version}version} of this implementation of the PSA Crypto API. *)

val crypto_init : unit -> Status.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/library/library.html#c.psa_crypto_init}[psa_crypto_init]}

    {b Note.} A call to this function is made in the initialisation
    of the {!Bytesrw_crypto} module. *)

(** {1:algorithms
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html}Algorithms}} *)

(** Algorithms. *)
module Alg : sig

  (** {1:algorithms Algorithms} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.psa_algorithm_t}[psa_algorithm_t]}. *)

  val none : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_NONE}[PSA_ALG_NONE]}. *)

  (** {1:algorithm_categories
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#algorithm-categories}Algorithm categories}}  *)

  val is_hash : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_HASH}[PSA_ALG_IS_HASH]}. *)

  val is_mac : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_MAC}[PSA_ALG_IS_MAC]}. *)

  val is_cipher : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_CIPHER}[PSA_ALG_IS_CIPHER]}. *)

  val is_aead : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_AEAD}[PSA_ALG_IS_AEAD]}. *)

  val is_key_derivation : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_KEY_DERIVATION}[PSA_ALG_IS_KEY_DERIVATION]}. *)

  val is_sign : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_SIGN}[PSA_ALG_IS_SIGN]}. *)

  val is_asymmetric_encryption : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_ASYMMETRIC_ENCRYPTION}[PSA_ALG_IS_ASYMMETRIC_ENCRYPTION]}. *)

  val is_key_agreement : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_KEY_AGREEMENT}[PSA_ALG_IS_KEY_AGREEMENT]}. *)


  val is_wildcard : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_IS_WILDCARD}[PSA_ALG_IS_WILDCARD]}. *)

  val get_hash : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/algorithms.html#c.PSA_ALG_GET_HASH}[PSA_ALG_GET_HASH]}. *)

  (** {1:hashes
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#hash-algorithms}Message digests (Hashes)}} *)

  val md2 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_MD2}[PSA_ALG_MD2]} {b WARNING} Only use for legacy applications. *)

  val md4 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_MD4}[PSA_ALG_MD4]} {b WARNING} Only use for legacy applications. *)

  val md5 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_MD5}[PSA_ALG_MD5]} {b WARNING} Only use for legacy applications. *)

  val ripemd160 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_RIPEMD160}[PSA_ALG_RIPEMD160]} *)

  val aes_mmo_zigbee : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_AES_MMO_ZIGBEE}[PSA_ALG_AES_MMO_ZIGBEE]} *)

  val sha_1 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA_1}[PSA_ALG_SHA_1]} {b WARNING} Only use for legacy applications. *)

  val sha_224 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA_224}[PSA_ALG_SHA_224]} *)

  val sha_256 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA_256}[PSA_ALG_SHA_256]} *)

  val sha_384 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA_384}[PSA_ALG_SHA_384]} *)

  val sha_512 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA_512}[PSA_ALG_SHA_512]} *)

  val sha_512_224 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA_512_224}[PSA_ALG_SHA_512_224]} *)

  val sha_512_256 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA_512_256}[PSA_ALG_SHA_512_256]} *)

  val sha3_224 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA3_224}[PSA_ALG_SHA3_224]} *)

  val sha3_256 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA3_356}[PSA_ALG_SHA3_256]} *)

  val sha3_384 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA3_384}[PSA_ALG_SHA3_384]} *)

  val sha3_512 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHA3_512}[PSA_ALG_SHA3_512]} *)

  val shake256_512 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SHAKE256_512}[PSA_ALG_SHAKE256_512]} *)

  val sm3 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_ALG_SM3}[PSA_ALG_SM3]} *)

  (** {1:macs
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html}
      Message authentication codes (MAC)}} *)

  val hmac : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_HMAC}[PSA_ALG_HMAC]} *)

  val cbc_mac : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_CBC_MAC}[PSA_ALG_CBC_MAC]} {b WARNING} Not recommended. *)

  val cbc : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_CBC}[PSA_ALG_CBC]} *)

  val truncated_mac : t -> length:int -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_TRUNCATED_MAC}[PSA_ALG_TRUNCATED_MAC]} *)

  val full_length_mac : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_FULL_LENGTH_MAC}[PSA_ALG_FULL_LENGTH_MAC]} *)

  val at_least_this_length_mac : t -> length:int -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_AT_LEAST_THIS_LENGTH_MAC}[PSA_ALG_AT_LEAST_THIS_LENGTH_MAC]} *)

  val is_hmac : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_IS_HMAC}[PSA_ALG_IS_HMAC]} *)

  val is_block_cipher_mac : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_IS_BLOCK_CIPHER_MAC}[PSA_ALG_IS_BLOCK_CIPHER_MAC]}. *)

  (** {1:unauthciphers
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html}Unauthenticated ciphers}} *)

  val stream_cipher : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_STREAM_CIPHER}[PSA_ALG_STREAM_CIPHER]}. *)

  val ctr : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_CTR}[PSA_ALG_CTR]} *)

  val ccm_star_no_tag : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_CCM_STAR_NO_TAG}[PSA_ALG_CCM_STAR_NO_TAG]} *)

  val ccm_star_any_tag : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_CCM_STAR_ANY_TAG}[PSA_ALG_CCM_STAR_ANY_TAG]} *)

  val cfb : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_CFB}[PSA_ALG_CFB]} *)

  val ofb : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_OFB}[PSA_ALG_OFB]} *)

  val xts : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_XTS}[PSA_ALG_XTS]} *)

  val ecb_no_padding : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_ECB_NO_PADDING}[PSA_ALG_ECB_NO_PADDING]} *)

  val cbc_no_padding : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_CBC_NO_PADDING}[PSA_ALG_CBC_NO_PADDING]} *)

  val cbc_pkcs7 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_CBC_PKCS7}[PSA_ALG_CBC_PKCS7]} *)

  val is_stream_cipher : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_ALG_IS_STREAM_CIPHER}[PSA_ALG_IS_STREAM_CIPHER]} *)

  (** {1:aead
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html}
      Authenticated encryption with associcated data (AEAD)}} *)

  val ccm : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_CCM}[PSA_ALG_CCM]} *)

  val gcm : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_GCM}[PSA_ALG_GCM]} *)

  val chacha20_poly1305 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_CHACHA20_POLY1305}[PSA_ALG_CHACHA20_POLY1305]} *)

  val xchacha20_poly1305 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_XCHACHA20_POLY1305}[PSA_ALG_XCHACHA20_POLY1305]} *)

  val aead_with_shortened_tag : t -> length:int -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_AEAD_WITH_SHORTENED_TAG}[PSA_ALG_AEAD_WITH_SHORTENED_TAG]} *)

  val aead_with_default_length_tag : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_AEAD_WITH_DEFAULT_LENGTH_TAG}[PSA_ALG_AEAD_WITH_DEFAULT_LENGTH_TAG]} *)

  val aead_with_at_least_this_length_tag : t -> length:int -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_AEAD_WITH_AT_LEAST_THIS_LENGTH_TAG}[PSA_ALG_AEAD_WITH_AT_LEAST_THIS_LENGTH_TAG]} *)

  val is_aead_on_block_cipher : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_ALG_IS_AEAD_ON_BLOCK_CIPHER}[PSA_ALG_IS_AEAD_ON_BLOCK_CIPHER]} *)

  (** {1:key_derivation {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html}Key derivation}} *)

  val hkdf : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_HKDF}[PSA_ALG_HKDF]} *)

  val hkdf_extract : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_HKDF_EXTRACT}[PSA_ALG_HKDF_EXTRACT]} *)

  val hkdf_expand : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_HKDF_EXPAND}[PSA_ALG_HKDF_EXPAND]} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val sp800_108_counter_hmac : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_SP800_108_COUNTER_HMAC}[PSA_ALG_SP800_108_COUNTER_HMAC]} *) *)

  (* Not in TF-PSA-Crypto 1.0.0
  val sp800_108_counter_cmac : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_SP800_108_COUNTER_CMAC}[PSA_ALG_SP800_108_COUNTER_CMAC]} *) *)

  val tls12_prf : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_TLS12_PRF}[PSA_ALG_TLS12_PRF]} *)

  val tls12_psk_to_ms : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_TLS12_PSK_TO_MS}[PSA_ALG_TLS12_PSK_TO_MS]} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val tls12_ecjpake_to_pms : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_TLS12_ECJPAKE_TO_PMS}[PSA_ALG_TLS12_ECJPAKE_TO_PMS]} *) *)

  val pbkdf2_hmac : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_PBKDF2_HMAC}[PSA_ALG_PBKDF2_HMAC]} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val pbkdf2_aes_cmac_prf_128 : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_PBKDF2_AES_CMAC_PRF_128}[PSA_ALG_AES_CMAC_PRF_128]} *) *)

  val is_key_derivation_stretching : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_KEY_DERIVATION_STRETCHING}[PSA_ALG_IS_KEY_DERIVATION_STRETCHING]} *)

  val is_hkdf : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_HKDF}[PSA_ALG_IS_HKDF]} *)

  val is_hkdf_extract : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_HKDF_EXTRACT}PSA_ALG_IS_HKDF_EXTRACT} *)

  val is_hkdf_expand : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_HKDF_EXPAND}PSA_ALG_IS_HKDF_EXPAND} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val is_sp800_108_counter_hmac : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_SP800_108_COUNTER_HMAC}[PSA_ALG_IS_SP800_108_COUNTER_HMAC] *) *)

  val is_tls12_prf : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_TLS12_PRF}[PSA_ALG_IS_TLS12_PRF]} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val is_tls12_psk_to_ms : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_TLS12_PSK_TO_MS}[PSA_ALG_IS_TLS12_SK_TO_MS] *) *)

  val is_pbkdf2_hmac : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_ALG_IS_PBKDF2_HMAC}[PSA_ALG_IS_PBKDF2_HMAC]} *)

(** {1:asymmetric_signature
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html}
    Asymmetric signature}} *)

  val rsa_pkcs1v15_sign : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_RSA_PKCS1V15_SIGN}[PSA_ALG_RSA_PKCS1V15_SIGN]} *)

  val rsa_pkcs1v15_sign_raw : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_RSA_PKCS1V15_SIGN_RAW}[PSA_ALG_RSA_PKCS1V15_SIGN_RAW]} *)

  val rsa_pss : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_RSA_PSS}[PSA_ALG_RSA_PSS]} *)

  val rsa_pss_any_salt : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_RSA_PSS_ANY_SALT}[PSA_ALG_RSA_PSS_ANY_SALT]} *)

  val ecdsa : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_ECDSA}[PSA_ALG_ECDSA]} *)

  val ecdsa_any : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_ECDSA_ANY}[PSA_ALG_ECDSA_ANY]} *)

  val deterministic_ecdsa : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_DETERMINISTIC_ECDSA}[PSA_ALG_DETERMINISTIC_ECDSA]} *)

  val pure_eddsa : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_PURE_EDDSA}[PSA_ALG_PURE_EDDSA]} *)

  val ed25519ph : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_ED25519PH}[PSA_ALG_ED25519PH]} *)

  val ed448ph : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_ED448PH}[PSA_ALG_ED448PH]} *)

  val is_sign_message : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_SIGN_MESSAGE}[PSA_ALG_IS_SIGN_MESSAGE]} *)

  val is_sign_hash : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_SIGN_HASH}[PSA_ALG_IS_SIGN_HASH]} *)

  val is_rsa_pkcs1v15_sign : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_RSA_PKCS1V15_SIGN}[PSA_ALG_IS_RSA_PKCS1V15_SIGN]} *)

  val is_rsa_pss : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_RSA_PSS}[PSA_ALG_IS_RSA_PSS]} *)

  val is_rsa_pss_any_salt : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_RSA_PSS_ANY_SALT}[PSA_ALG_IS_RSA_PSS_ANY_SALT]} *)

  val is_rsa_pss_standard_salt : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_RSA_PSS_STANDARD_SALT}[PSA_ALG_IS_RSA_PSS_STANDARD_SALT]} *)

  val is_ecdsa : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_ECDSA}[PSA_ALG_IS_ECDSA]} *)

  val is_deterministic_ecdsa : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_DETERMINISTIC_ECDSA}[PSA_ALG_IS_DETERMINISTIC_ECDSA]} *)

  val is_randomized_ecdsa : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_RANDOMIZED_ECDSA}[PSA_ALG_IS_RANDOMIZED_ECDSA]} *)

  val is_hash_eddsa : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_HASH_EdDSA}[PSA_ALG_IS_HASH_EDDSA]} *)

  val is_hash_and_sign : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_IS_HASH_AND_SIGN}[PSA_ALG_IS_HASH_AND_SIGN]} *)

  val any_hash : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_ALG_ANY_HASH}[PSA_ALG_ANY_HASH]} *)

  (** {1:asymmetric_encryption
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html}
      Asymmetric encryption}} *)

  val rsa_pkcs1v15_crypt : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.PSA_ALG_RSA_PKCS1V15_CRYPT}[PSA_ALG_RSA_PKCS1V15_CRYPT]} *)

  val rsa_oaep : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.PSA_ALG_RSA_OAEP}[PSA_ALG_RSA_OAEP]} *)

  val is_rsa_oaep : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.PSA_ALG_IS_RSA_OAEP}[PSA_ALG_IS_RSA_OAEP]} *)

  (** {1:key_agreement
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html}
      Key agreement}} *)

  val ffdh : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_FFDH}[PSA_ALG_FFDH]} *)

  val ecdh : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_ECDH}[PSA_ALG_ECDH]} *)

  val key_agreement : ka:t -> kdf:t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_KEY_AGREEMENT}[PSA_ALG_KEY_AGREEMENT]} *)

  val key_agreement_get_base : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_KEY_AGREEMENT_GET_BASE}[PSA_ALG_KEY_AGREEMENT_GET_BASE]} *)

  val key_agreement_get_kdf : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_KEY_AGREEMENT_GET_KDF}[PSA_ALG_KEY_AGREEMENT_GET_KDF]} *)

  val is_standalone_key_agreement : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_IS_STANDALONE_KEY_AGREEMENT}[PSA_ALG_IS_STANDALONE_KEY_AGREEMENT]} *)

  val is_ffdh : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_IS_FFDH}[PSA_ALG_IS_FFHD]} *)

  val is_ecdh : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_ALG_IS_ECDH}[PSA_ALG_IS_ECDH]} *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal a0 a1] is [true] iff [a0] and [a1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:converting Converting} *)

  val to_uint32 : t -> uint32
  (** [to_uint32 a] is [a] as an unsigned 32-bit integer. *)

  val of_uint32 : uint32 -> t
  (** [of_uint32 i] is [i] as an algorithm. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats algorithms for inspection. *)
end

(** {1:key_management
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/index.html}
    Key management}} *)

(** {2:key_ids
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html}
    Key identifiers}} *)

(** Key identifiers. *)
module Key_id : sig

  (** {1:key_ids Key identifiers} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.psa_key_id_t}[psa_key_id_t]}.

      Note that the key identifiers you create must be manually disposed
      of with {!Bytesrw_crypto.Psa.destroy_key}. *)

  val null : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.PSA_KEY_ID_NULL}[PSA_KEY_ID_NULL]} *)

  val user_min : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.PSA_KEY_ID_USER_MIN}[PSA_KEY_ID_USER_MIN]} *)

  val user_max : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.PSA_KEY_ID_USER_MAX}[PSA_KEY_ID_USER_MAX]} *)

  val vendor_min : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.PSA_KEY_ID_VENDOR_MIN}[PSA_KEY_ID_VENDOR_MIN]} *)

  val vendor_max : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.PSA_KEY_ID_VENDOR_MAX}[PSA_KEY_ID_VENDOR_MAX]} *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal k0 k1] is [true] iff [k0] and [k1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint32 : t -> uint32
  (** [to_uint32 k] is [k] as an unsigned 32-bit integer. *)

  val of_uint32 : uint32 -> t
  (** [of_uint32 i] is a key identifier from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats key identifiers for inspection. *)
end

(** {2:key_attributes
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/attributes.html}Key attributes}} *)

(** Key attributes. *)
module Key_attributes : sig

  (** {1:key_attributes Key attributes} *)

  type t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/attributes.html#c.psa_key_attributes_t}[psa_key_attributes_t]}.

        {b Note.} These values are finalized by the garbage collector
        via a call to {!Psa.reset_key_attributes}. *)

  val init : unit -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/attributes.html#c.psa_key_attributes_init}[psa_key_attributes_init]} *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats key attributes for inspection. *)
end

val get_key_attributes : Key_id.t -> Key_attributes.t -> Status.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/attributes.html#c.psa_get_key_attributes}[psa_get_key_attributes]} *)

val reset_key_attributes : Key_attributes.t -> unit
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/attributes.html#c.psa_reset_key_attributes}[psa_reset_key_attributes]} *)

val get_key_id : Key_attributes.t -> Key_id.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.psa_get_key_id}[psa_get_key_id]} *)

val set_key_id : Key_attributes.t -> Key_id.t -> unit
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/ids.html#c.psa_set_key_id}[psa_set_key_id]} *)


(** {2:key_lifetimes
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html}
    Key lifetimes}} *)

(** Key locations. *)
module Key_location : sig

  (** {1:key_locations Key locations} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.psa_key_location_t}[psa_key_location_t].} *)

  val local_storage : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LOCATION_LOCAL_STORAGE}[PSA_KEY_LOCATION_LOCAL_STORAGE]} *)

  val primary_secure_element : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LOCATION_PRIMARY_SECURE_ELEMENT}[PSA_KEY_LOCATION_PRIMARY_SECURE_ELEMENT]} *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal l0 l1] is [true] iff [l0] and [l1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint32 : t -> uint32
  (** [to_uint32 l] is [l] as an unsigned 32-bit integer. *)

  val of_uint32 : uint32 -> t
  (** [of_uint32 i] is a key location from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats key locations for inspection. *)
end

(** Key persistence. *)
module Key_persistence : sig

  (** {1:key_persistence Key persistence} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.psa_key_persistence_t}[psa_key_persistence_t]}. *)

  val volatile : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_PERSISTENCE_VOLATILE}[PSA_KEY_PERSISTENCE_VOLATILE]} *)

  val default : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_PERSISTENCE_DEFAULT}[PSA_KEY_PERSISTENCE_DEFAULT]} *)

  val read_only : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_PERSISTENCE_READ_ONLY}[PSA_KEY_PERSISTENCE_READ_ONLY]} *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal p0 p1] is [true] iff [p0] and [p1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint8 : t -> uint8
  (** [to_uint8 p] is [p] as an unsigned 8-bit integer. *)

  val of_uint8 : uint8 -> t
  (** [of_uint8 i] is a key persistence from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats key persistence for inspection. *)
end

(** Key lifetimes. *)
module Key_lifetime : sig

  (** {1:key_lifetimes Key lifetimes} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.psa_key_lifetime_t}[psa_key_lifetime_t]}. *)

  val volatile : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LIFETIME_VOLATILE}[PSA_KEY_LIFETIME_VOLATILE]} *)

  val persistent : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LIFETIME_PERSISTENT}[PSA_KEY_LIFETIME_PERSISTENT]} *)

  val from_persistence_and_location : Key_persistence.t -> Key_location.t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LIFETIME_FROM_PERSISTENCE_AND_LOCATION}[PSA_KEY_LIFETIME_FROM_PERSISTENCE_AND_LOCATION]} *)

  val get_persistence : t -> Key_persistence.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LIFETIME_GET_PERSISTENCE}[PSA_KEY_LIFETIME_GET_PERSISTENCE]} *)

  val get_location : t -> Key_location.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LIFETIME_GET_LOCATION}[PSA_KEY_LIFETIME_GET_LOCATION]} *)

  (** {1:preds Predicates and comparisons} *)

  val is_volatile : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.PSA_KEY_LIFETIME_IS_VOLATILE}[PSA_KEY_LIFETIME_IS_VOLATILE]}. *)

  val equal : t -> t -> bool
  (** [equal l0 l1] is [true] iff [l0] and [l1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint32 : t -> uint32
  (** [to_uint32 l] is [l] as an unsigned 32-bit integer. *)

  val of_uint32 : uint32 -> t
  (** [of_uint32 i] is a key lifetime from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats key lifetimes for inspection. *)
end

val get_key_lifetime : Key_attributes.t -> Key_lifetime.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.psa_get_key_lifetime}[psa_get_key_lifetime]} *)

val set_key_lifetime : Key_attributes.t -> Key_lifetime.t -> unit
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/lifetimes.html#c.psa_set_key_lifetime}[psa_set_key_lifetime]} *)

(** {2:key_usages
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#key-usage-flags}
    Key usages}} *)

(** Key usages. *)
module Key_usage : sig

  (** {1:key_usages Key usages} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.psa_key_usage_t}[psa_key_usage_t]}. *)

  val ( + ) : t -> t -> t
  (** [u0 + u1] combines usages [u0] with those of [u1]. *)

  val ( - ) : t -> t -> t
  (** [u0 - u1] removes usages [u1] from those of [u0]. *)

  val none : t
  (** [none] has no usage specified. *)

  val export : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_EXPORT}[PSA_KEY_USAGE_EXPORT]} *)

  val copy : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_COPY}[PSA_KEY_USAGE_COPY]} *)

  val cache : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_CACHE}[PSA_KEY_USAGE_CACHE]} *)

  val encrypt : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_ENCRYPT}[PSA_KEY_USAGE_ENCRYPT]} *)

  val decrypt : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_DECRYPT}[PSA_KEY_USAGE_DECRYPT]} *)

  val sign_message : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_SIGN_MESSAGE}[PSA_KEY_USAGE_SIGN_MESSAGE]} *)

  val verify_message : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_VERIFY_MESSAGE}[PSA_KEY_USAGE_VERIFY_MESSAGE]} *)

  val sign_hash : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_SIGN_HASH}[PSA_KEY_USAGE_SIGN_HASH]} *)

  val verify_hash : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_VERIFY_HASH}[PSA_KEY_USAGE_VERIFY_HASH]} *)

  val derive : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_DERIVE}[PSA_KEY_USAGE_DERIVE]} *)

  val verify_derivation : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.PSA_KEY_USAGE_VERIFY_DERIVATION}[PSA_KEY_VERIFY_DERIVATION]} *)

  (** {1:preds Predicates and comparisons} *)

  val test : has:t -> t -> bool
  (** [test ~has u] is [true] iff [u] has at the usages of [has]. *)

  val equal : t -> t -> bool
  (** [equal l0 l1] is [true] iff [l0] and [l1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint32 : t -> uint32
  (** [to_uint32 l] is [l] as an unsigned 32-bit integer. *)

  val of_uint32 : uint32 -> t
  (** [of_uint32 i] is a key usage from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats key usages for inspection. *)
end

val get_key_algorithm : Key_attributes.t -> Alg.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.psa_get_key_algorithm}[psa_get_key_algorithm]} *)

val set_key_algorithm : Key_attributes.t -> Alg.t -> unit
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.psa_set_key_algorithm}[psa_set_key_algorithm]} *)

val get_key_usage_flags : Key_attributes.t -> Key_usage.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.psa_get_key_usage_flags}[psa_get_key_usage_flags]} *)

val set_key_usage_flags : Key_attributes.t -> Key_usage.t -> unit
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/policy.html#c.psa_set_key_usage_flags}[psa_set_key_usage_flags]} *)

(** {2:key_types
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html}
      Key types}} *)

(** Elliptic curve keys. *)
module Ecc_family : sig

  (** {1:elliptic_curve_families Elliptic curve families} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.psa_ecc_family_t}[psa_ecc_family_t]}. *)

  val secp_k1 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_SECP_K1}[PSA_ECC_FAMILY_SECP_K1]} *)

  val secp_r1 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_SECP_R1}[PSA_ECC_FAMILY_SECP_R1]} *)

  val secp_r2 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_SECP_R2}[PSA_ECC_FAMILY_SECP_R2]}
      {b WARNING} weak and deprecated. *)

  val sect_k1 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_SECT_K1}[PSA_ECC_FAMILY_SECT_K1]}
      {b WARNING} [sect163k1] is weak and deprecated. *)

  val sect_r1 : t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_SECT_R1}[PSA_ECC_FAMILY_SECT_R1]}
    {b WARNING} [sect163r1] is weak and deprecated. *)

  val sect_r2 : t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_SECT_R2}[PSA_ECC_FAMILY_SECT_R2]}
    {b WARNING} [sect163r2] is weak and deprecated. *)

  val brainpool_p_r1 : t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_BRAINPOOL_P_R1}[PSA_ECC_FAMILY_BRAINPOOL_P_R1]}
    {b WARNING} [brainpoolP160r1] is weak and deprecated. *)

  val frp : t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_FRP}[PSA_ECC_FAMILY_FRP]} *)

  val montgomery : t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_MONTGOMERY}[PSA_ECC_FAMILY_MONTGOMERY]} *)

  val twisted_edwards : t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_ECC_FAMILY_TWISTED_EDWARDS}[PSA_ECC_FAMILY_TWISTED_EDWARDS]} *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal f0 f1] is [true] iff [f0] and [f1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint8 : t -> uint8
  (** [to_uint8 f] is [f] as an unsigned 8-bit integer. *)

  val of_uint8 : uint8 -> t
  (** [of_uint8 i] is an elliptic curve family identifier from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats an elliptic curve family identifier for inspection. *)
end

(** Diffie-Hellman keys. *)
module Dh_family : sig

  (** {1:diffie_hellman Diffie-Hellman family} *)

  type t
  (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.psa_dh_family_t}[psa_dh_family_t]}. *)

  val rfc7919 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_DH_FAMILY_RFC7919}[PSA_DH_FAMILY_RFC7919]} *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal f0 f1] is [true] iff [f0] and [f1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint8 : t -> uint8
  (** [to_uint8 f] is [f] as an unsigned 8-bit integer. *)

  val of_uint8 : uint8 -> t
  (** [of_uint8 i] is a Diffie-Hellman family identifier from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a Diffie-Hellman family identifier for inspection. *)
end

(** Key types. *)
module Key_type : sig

  (** {1:key_types Key types} *)

  type t
  (** The type for for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.psa_key_type_t}[psa_key_type]}. *)

  val none : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_NONE}[PSA_KEY_TYPE_NONE]}. *)

  (** {1:key_categories
        {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#key-categories}Key categories}} *)

  val is_unstructured : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_UNSTRUCTURED}[PSA_KEY_TYPE_IS_UNSTRUCTURED]} *)

  val is_asymmetric : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_ASYMMETRIC}[PSA_KEY_TYPE_IS_ASYMMETRIC]} *)

  val is_public_key : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_PUBLIC_KEY}[PSA_KEY_TYPE_IS_PUBLIC_KEY]} *)

  val is_key_pair : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_KEY_PAIR}[PSA_KEY_TYPE_IS_KEY_PAIR]} *)

  (** {1:symmetric_keys
        {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#symmetric-keys}Symmetric keys}} *)

  val raw_data : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_RAW_DATA}[PSA_KEY_TYPE_RAW_DATA]} *)

  val hmac : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_HMAC}[PSA_KEY_TYPE_HMAC]} *)

  val derive : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_DERIVE}[PSA_KEY_TYPE_DERIVE]} *)

  val password : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_PASSWORD}[PSA_KEY_TYPE_PASSWORD]} *)

  val password_hash : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_PASSWORD_HASH}[PSA_KEY_TYPE_PASSWORD_HASH]} *)

  val pepper : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_PEPPER}[PSA_KEY_TYPE_PEPPER]} *)

  val aes : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_AES}[PSA_KEY_TYPE_AES]} *)

  val aria : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_ARIA}[PSA_KEY_TYPE_ARIA]} *)

  val des : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_DES}[PSA_KEY_TYPE_DES]} {b WARNING} Weak and
        strongly deprecated. *)

  val camellia : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_CAMELLIA}[PSA_KEY_TYPE_CAMELLIA]} *)

  val sm4 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_SM4}[PSA_KEY_TYPE_SM4]} *)

  val arc4 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_ARC4}[PSA_KEY_TYPE_ARC4]} {b WARNING} Weak and strongly
        deprecated.  *)

  val chacha20 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_CHACHA20}[PSA_KEY_TYPE_CHACHA20]} *)

  val xchacha20 : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_XCHACHA20}[PSA_KEY_TYPE_XCHACHA20]} *)

  (** {1:rsa_keys
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#rsa-keys}RSA keys}} *)

  val rsa_key_pair : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_RSA_KEY_PAIR}[PSA_KEY_TYPE_RSA_KEY_PAIR]} *)

  val rsa_public_key : t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_RSA_PUBLIC_KEY}[PSA_KEY_TYPE_RSA_PUBLIC_KEY]} *)

  val is_rsa : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_RSA}[PSA_KEY_TYPE_IS_RSA]} *)

  (** {1:ecc_keys
        {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#elliptic-curve-keys}Elliptic curve keys}}

  See also {!Ecc_family}. *)

  val ecc_key_pair : Ecc_family.t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_ECC_KEY_PAIR}[PSA_KEY_TYPE_ECC_KEY_PAIR]} *)

  val ecc_public_key : Ecc_family.t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_ECC_PUBLIC_KEY}[PSA_KEY_TYPE_ECC_PUBLIC_KEY]} *)

  val is_ecc : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_ECC}[PSA_KEY_TYPE_IS_ECC]} *)

  val is_ecc_key_pair : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_ECC_KEY_PAIR}[PSA_KEY_TYPE_IS_ECC_KEY_PAIR]} *)

  val is_ecc_public_key : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_ECC_PUBLIC_KEY}[PSA_KEY_TYPE_IS_ECC_PUBLIC_KEY]} *)

  val ecc_get_family : t -> Ecc_family.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_ECC_GET_FAMILY}[PSA_KEY_TYPE_ECC_GET_FAMILY]} *)

  (** {1:dh_keys
        {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#diffie-hellman-keys}Diffie-Hellman keys}}

  See also {!Dh_family}. *)

  val dh_key_pair : Dh_family.t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_DH_KEY_PAIR}[PSA_KEY_TYPE_DH_KEY_PAIR]} *)

  val dh_public_key : Dh_family.t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_DH_PUBLIC_KEY}[PSA_KEY_TYPE_DH_PUBLIC_KEY]} *)

  val is_dh : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_DH}[PSA_KEY_TYPE_IS_DH]} *)

  val is_dh_key_pair : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_DH_KEY_PAIR}[PSA_KEY_TYPE_IS_DH_KEY_PAIR]} *)

  val is_dh_public_key : t -> bool
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_IS_DH_PUBLIC_KEY}[PSA_KEY_TYPE_IS_DH_PUBLIC_KEY]} *)

  val dh_get_family : t -> Dh_family.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_DH_GET_FAMILY}[PSA_KEY_TYPE_DH_GET_FAMILY]} *)

  (** {1:g_pub_key Generic public keys} *)

  val key_pair_of_public_key : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_KEY_PAIR_OF_PUBLIC_KEY}[PSA_KEY_TYPE_KEY_PAIR_OF_PUBLIC_KEY]} *)

  val public_key_of_key_pair : t -> t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.PSA_KEY_TYPE_PUBLIC_KEY_OF_KEY_PAIR}[PSA_KEY_TYPE_PUBLIC_KEY_OF_KEY_PAIR]} *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal t0 t1] is [true] iff [t0] and [t1] are equal. *)

  val compare : t -> t -> int
  (** [compare] is a total order compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val to_uint16 : t -> uint16
  (** [to_uint16 t] is [t] as an unsigned 16-bit integer. *)

  val of_uint16 : uint16 -> t
  (** [of_uint16 i] is a key type from [i]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats key types for inspection. *)
end

val get_key_type : Key_attributes.t -> Key_type.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.psa_get_key_type}[psa_get_key_type]} *)

val set_key_type : Key_attributes.t -> Key_type.t -> unit
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.psa_set_key_type}[psa_set_key_type]} *)

val get_key_bits : Key_attributes.t -> int
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.psa_get_key_bits}[psa_get_key_bits]} *)

val set_key_bits : Key_attributes.t -> int -> unit
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/types.html#c.psa_set_key_bits}[psa_set_key_bits]} *)


(** {2:key_creation
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#key-creation}Creation}} *)

val import_key :
  ?length:int -> Key_attributes.t -> bigbytes -> (Key_id.t, Status.t) result
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.psa_import_key}[psa_import_key]} is [Ok kid] if the key was imported by
    reading [length] bytes of the bigbytes (defaults to the size of
    the bigbytes). *)

val generate_key : Key_attributes.t -> (Key_id.t, Status.t) result
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.psa_generate_key}[psa_generate_key]} is [Ok kid] if the was generated. *)

val copy_key : Key_id.t -> Key_attributes.t -> (Key_id.t, Status.t) result
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.psa_copy_key}[psa_copy_key]} is [Ok kid] if the key was copied. *)

(** {2:key_destruction
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#key-destruction}Destruction}} *)

val destroy_key : Key_id.t -> Status.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.psa_destroy_key}[psa_destroy_key]} *)

val purge_key : Key_id.t -> Status.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.psa_purge_key}[psa_purge_key]} *)

(** {2:key_export
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#key-export}Export}} *)

val export_key : Key_id.t -> bigbytes -> (int, Status.t) result
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.psa_export_key}[psa_export_key]} is [Ok n] iff [n] bytes were written.  *)

val export_public_key : Key_id.t -> bigbytes -> (int, Status.t) result
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.psa_export_public_key}[psa_export_public_key]} is
    [Ok n] iff [n] bytes were written. *)

val export_key_output_size : Key_type.t -> bits:int -> int
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.PSA_EXPORT_KEY_OUTPUT_SIZE}[PSA_EXPORT_KEY_OUTPUT_SIZE]} *)

val export_public_key_output_size : Key_type.t -> bits:int -> int
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.PSA_EXPORT_PUBLIC_KEY_OUTPUT_SIZE}[PSA_EXPORT_PUBLIC_KEY_OUTPUT_SIZE]} *)

val export_key_pair_max_size : unit -> int
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.PSA_EXPORT_PUBLIC_KEY_MAX_SIZE}[PSA_EXPORT_KEY_PAIR_MAX_SIZE]} *)

val export_public_key_max_size : unit -> int
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/keys/management.html#c.PSA_EXPORT_PUBLIC_KEY_MAX_SIZE}[PSA_EXPORT_PUBLIC_KEY_MAX_SIZE]} *)

(** {1:crypto_ops
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/index.html}
    Cryptographic operations}}  *)

(** {2:message_digests {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html}Message digests (Hashes)}} *)

(** Message digests.

    See also {!Alg.hashes} in [Alg]. *)
module Hash : sig

  (** {1:single_part Single-part functions} *)

  val compute :
    Alg.t -> input:Bytes.Slice.t -> hash:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_compute}[psa_hash_compute]}. *)

  val compare :
    Alg.t -> input:Bytes.Slice.t -> hash:Bytes.Slice.t -> Status.t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_compare}[psa_hash_compare]} *)

  (** {1:multi_part Multi-part functions} *)

  (** Hash operations. *)
  module Operation : sig
    type t
    (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_operation_t}[psa_hash_operation_t]}.

    {b Note.} These values are finalized by the garbage collector
    via a call to {!Bytesrw_crypto.Psa.Hash.abort}. *)

    val init : unit -> t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_operation_init}[psa_hash_operation_init]} *)
  end

  val setup : Operation.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_setup}[psa_hash_setup]} *)

  val update : Operation.t -> Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_update}[psa_hash_update]} *)

  val finish : Operation.t -> hash:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_finish}[psa_hash_finish]} *)

  val verify : Operation.t -> hash:Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_verify}[psa_hash_verify]} *)

  val abort : Operation.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_abort}[psa_hash_abort]} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val suspend : Operation.t -> state:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_suspend}[psa_hash_suspend]} *) *)

  (* Not in TF-PSA-Crypto 1.0.0
  val resume : Operation.t -> state:Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_resume}[psa_hash_resume]} *) *)

  val clone : src:Operation.t -> dst:Operation.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.psa_hash_clone}[psa_hash_clone]} *)

  (** {1:support Support functions} *)

  val length : Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_LENGTH}PSA_HASH_LENGTH} *)

  val max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_MAX_SIZE}PSA_HASH_MAX_SIZE} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val suspend_output_size : Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_SUSPEND_OUTPUT_SIZE}PSA_HASH_SUSPEND_OUTPUT_SIZE} *) *)

  (* Not in TF-PSA-Crypto 1.0.0
  val suspend_output_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_SUSPEND_OUTPUT_MAX_SIZE}PSA_HASH_SUSPEND_OUTPUT_MAX_SIZE} *) *)

  (* Not in TF-PSA-Crypto 1.0.0
  val suspend_algorithm_field_length : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_SUSPEND_ALGORITHM_FIELD_LENGTH}PSA_HASH_SUSPEND_ALGORITHM_FIELD_LENGTH} *) *)

  (* Not in TF-PSA-Crypto 1.0.0
  val suspend_input_length_field_length : Alg.t -> int
     (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_SUSPEND_INPUT_LENGTH_FIELD_LENGTH}PSA_HASH_SUSPEND_INPUT_LENGTH_FIELD_LENGTH} *) *)

  (* Not in TF-PSA-Crypto 1.0.0
  val suspend_hash_state_field_length : Alg.t -> int
     (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_SUSPEND_HASH_STATE_FIELD_LENGTH}PSA_HASH_SUPSPEND_HASH_STATE_FIELD_LENGTH} *) *)

  val block_length : Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/hashes.html#c.PSA_HASH_BLOCK_LENGTH}PSA_HASH_BLOCK_LENGTH} *)
end

(** {2:macs
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html}
    Message authentication codes (MAC)}} *)

(** Message authentication codes (MAC).

    See also {!Alg.macs} in [Alg]. *)
module Mac : sig

  (** {1:single_part Single-part functions} *)

  val compute :
    key:Key_id.t -> Alg.t -> input:Bytes.Slice.t -> mac:Bytes.Slice.t ->
    (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_compute}[psa_mac_compute]}. *)

  val verify :
    key:Key_id.t -> Alg.t -> input:Bytes.Slice.t -> mac:Bytes.Slice.t ->
    Status.t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_compute}[psa_mac_verify]}. *)

  (** {1:multi_part Multi-part functions} *)

  (** MAC operations. *)
  module Operation : sig
    type t
    (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_operation_t}[psa_mac_operation_t]}.

    {b Note.} These values are finalized by the garbage collector
    via a call to {!Bytesrw_crypto.Psa.Mac.abort}. *)

    val init : unit -> t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_operation_init}[psa_mac_operation_init]} *)
  end

  val sign_setup : Operation.t -> Key_id.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_sign_setup}[psa_mac_sign_setup]} *)

  val verify_setup : Operation.t -> Key_id.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_verify_setup}[psa_mac_verify_setup]} *)

  val update : Operation.t -> Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_update}[psa_mac_update]} *)

  val sign_finish : Operation.t -> hash:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_sign_finish}[psa_mac_sign_finish]} *)

  val verify_finish : Operation.t -> hash:Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_verify_finish}[psa_mac_verify_finish]} *)

  val abort : Operation.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.psa_mac_abort}[psa_mac_abort]} *)

  (** {1:support Support functions} *)

  val length : Key_type.t -> bits:int -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_MAC_LENGTH}PSA_MAC_LENGTH} *)

  val max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_MAC_MAX_SIZE}PSA_MAC_MAX_SIZE} *)
end

(** {2:unauth_ciphers
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html}
    Unauthenticated ciphers}} *)

(** Unauthenticated ciphers.

    See also {!Alg.unauthciphers} in [Alg].

    {b WARNING} It is recommended that newer protocols use
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html}
    Authenticated encryption with associated data (AEAD)}.     *)
module Cipher : sig

  (** {1:single_part Single-part functions} *)

  val encrypt :
    key:Key_id.t -> Alg.t -> plain:Bytes.Slice.t -> cipher:Bytes.Slice.t ->
    (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_encrypt}[psa_cipher_encrypt]} *)

  val decrypt :
    key:Key_id.t -> Alg.t -> cipher:Bytes.Slice.t -> plain:Bytes.Slice.t ->
    (int, Status.t) result
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_decrypt}[psa_cipher_decrypt]} *)

  (** {1:multi_part Multi-part functions} *)

  (** Cipher operations. *)
  module Operation : sig
    type t
    (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_operation_t}[psa_cipher_operation_t]}.

    {b Note.} These values are finalized by the garbage collector
    via a call to {!Bytesrw_crypto.Psa.Cipher.abort}. *)

    val init : unit -> t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_operation_init}[psa_cipher_operation_init]} *)
  end

  val encrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_encrypt_setup}[psa_cipher_encrypt_setup]} *)

  val decrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_encrypt_setup}[psa_cipher_decrypt_setup]} *)

  val generate_iv : Operation.t -> iv:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_generate_iv}[psa_cipher_generate_iv]} *)

  val set_iv : Operation.t -> iv:Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_set_iv}[psa_cipher_set_iv]} *)

  val update :
    Operation.t -> input:Bytes.Slice.t -> output:Bytes.Slice.t ->
    (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_update}[psa_cipher_update]} *)

  val finish : Operation.t -> output:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_finish}[psa_cipher_finish]} *)

  val abort : Operation.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.psa_cipher_abort}[psa_cipher_abort]} *)

  (** {1:support Support functions} *)

  val encrypt_output_size : Key_type.t -> Alg.t -> plain_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_ENCRYPT_OUTPUT_SIZE}[PSA_CIPHER_ENCRYPT_OUTPUT_SIZE]} *)

  val encrypt_output_max_size : plain_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_ENCRYPT_OUTPUT_MAX_SIZE}[PSA_CIPHER_ENCRYPT_OUTPUT_MAX_SIZE]} *)

  val decrypt_output_size : Key_type.t -> Alg.t -> cipher_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_DECRYPT_OUTPUT_SIZE}[PSA_CIPHER_DECRYPT_OUTPUT_SIZE]} *)

  val decrypt_output_max_size : cipher_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_DECRYPT_OUTPUT_MAX_SIZE}[PSA_CIPHER_DECRYPT_OUTPUT_MAX_SIZE]} *)

  val iv_length : Key_type.t -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_IV_LENGTH}[PSA_CIPHER_IV_LENGTH]} *)

  val iv_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_IV_MAX_SIZE}[PSA_CIPHER_IV_MAX_SIZE]} *)

  val update_output_size : Key_type.t -> Alg.t -> input_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_UPDATE_OUTPUT_SIZE}[PSA_CIPHER_UPDATE_OUTPUT_SIZE]} *)

  val update_output_max_size : input_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_UPDATE_OUTPUT_MAX_SIZE}[PSA_CIPHER_UPDATE_OUTPUT_MAX_SIZE]} *)

  val finish_output_size : Key_type.t -> Alg.t -> int
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_FINISH_OUTPUT_SIZE}[PSA_CIPHER_FINISH_OUTPUT_SIZE]} *)

  val finish_output_max_size : unit -> int
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_CIPHER_FINISH_OUTPUT_MAX_SIZE}[PSA_CIPHER_FINISH_OUTPUT_MAX_SIZE]} *)

  val block_length : Key_type.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_BLOCK_CIPHER_BLOCK_LENGTH}[PSA_BLOCK_CIPHER_BLOCK_LENGTH]} *)

  val block_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ciphers.html#c.PSA_BLOCK_CIPHER_BLOCK_MAX_SIZE}[PSA_BLOCK_CIPHER_BLOCK_MAX_SIZE]} *)
end

(** {2:auth_encryption
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html}
    Authenticated encryption with associated data (AEAD)}} *)

(** Authenticated encryption with associated data (AEAD).

    See also {!Alg.aead} in [Alg]. *)
module Aead : sig

  (** {1:single_part Single-part functions} *)

  val encrypt :
    key:Key_id.t -> Alg.t -> nonce:Bytes.Slice.t -> ad:Bytes.Slice.t ->
    plain:Bytes.Slice.t -> cipher:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_encrypt}[psa_aead_encrypt]} *)

  val decrypt :
    key:Key_id.t -> Alg.t -> nonce:Bytes.Slice.t -> ad:Bytes.Slice.t ->
    cipher:Bytes.Slice.t -> plain:Bytes.Slice.t ->
    (int, Status.t) result
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_decrypt}[psa_aead_decrypt]} *)

  (** {1:multi_part Multi-part functions} *)

  (** Cipher operations. *)
  module Operation : sig
    type t
    (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_operation_t}[psa_aead_operation_t]}.

    {b Note.} These values are finalized by the garbage collector
    via a call to {!Bytesrw_crypto.Psa.Aead.abort}. *)

    val init : unit -> t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_operation_init}[psa_aead_operation_init]} *)
  end

  val encrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_encrypt_setup}[psa_aead_encrypt_setup]} *)

  val decrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_decrypt_setup}[psa_aead_decrypt_setup]} *)

  val set_lengths : Operation.t -> ad_length:int -> plain_length:int -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_set_lengths}[psa_aead_set_lengths]} *)

  val generate_nonce :
    Operation.t -> nonce:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_generate_nonce}[psa_aead_generate_nonce]} *)

  val set_nonce : Operation.t -> nonce:Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_set_nonce}[psa_aead_set_nonce]} *)

  val update_ad : Operation.t -> ad:Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_update_ad}[psa_aead_update_ad]}. *)

  val update :
    Operation.t -> input:Bytes.Slice.t -> output:Bytes.Slice.t ->
    (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_update}[psa_aead_update]} *)

  val finish :
    Operation.t -> cipher:Bytes.Slice.t -> tag:Bytes.Slice.t ->
    (int * int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_finish}[psa_aead_finish]} *)

  val verify :
    Operation.t -> plain:Bytes.Slice.t -> tag:Bytes.Slice.t ->
    (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_verify}[psa_aead_verify]} *)

  val abort : Operation.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.psa_aead_abort}[psa_aead_abort]} *)

  (** {1:support Support functions} *)

    val encrypt_output_size : Key_type.t -> Alg.t -> plain_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_ENCRYPT_OUTPUT_SIZE}[PSA_AEAD_ENCRYPT_OUTPUT_SIZE]} *)

  val encrypt_output_max_size : plain_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_ENCRYPT_OUTPUT_MAX_SIZE}[PSA_AEAD_ENCRYPT_OUTPUT_MAX_SIZE]} *)

  val decrypt_output_size : Key_type.t -> Alg.t -> cipher_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_DECRYPT_OUTPUT_SIZE}[PSA_AEAD_DECRYPT_OUTPUT_SIZE]} *)

  val decrypt_output_max_size : cipher_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_DECRYPT_OUTPUT_MAX_SIZE}[PSA_AEAD_DECRYPT_OUTPUT_MAX_SIZE]} *)

  val nonce_length : Key_type.t -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_NONCE_LENGTH}[PSA_AEAD_NONCE_LENGTH]} *)

  val nonce_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_NONCE_MAX_SIZE}[PSA_AEAD_NONCE_MAX_SIZE]} *)

  val update_output_size : Key_type.t -> Alg.t -> input_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_UPDATE_OUTPUT_SIZE}[PSA_AEAD_UPDATE_OUTPUT_SIZE]} *)

  val update_output_max_size : input_length:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_UPDATE_OUTPUT_MAX_SIZE}[PSA_AEAD_UPDATE_OUTPUT_MAX_SIZE]} *)

  val finish_output_size : Key_type.t -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_FINISH_OUTPUT_SIZE}[PSA_AEAD_FINISH_OUTPUT_SIZE]} *)

  val finish_output_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_FINISH_OUTPUT_MAX_SIZE}[PSA_AEAD_FINISH_OUTPUT_MAX_SIZE]} *)

  val tag_length : Key_type.t -> bits:int -> Alg.t -> int
 (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_TAG_LENGTH}[PSA_AEAD_TAG_LENGTH]} *)

  val tag_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_TAG_MAX_SIZE}[PSA_AEAD_TAG_MAX_SIZE]} *)

  val verify_output_size : Key_type.t -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_VERIFY_OUTPUT_SIZE}[PSA_AEAD_VERIFY_OUTPUT_SIZE]} *)

  val verify_output_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/aead.html#c.PSA_AEAD_VERIFY_OUTPUT_MAX_SIZE}[PSA_AEAD_VERIFY_OUTPUT_MAX_SIZE]} *)
end

(** {2:key_derivation
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html}
    Key derivation}} *)

(** Key derivation.

    See also {!Alg.key_derivation} in [Alg]. *)
module Key_derivation : sig

  (** {1:input_steps Input step types} *)

  type step =
  | Input_secret
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_SECRET}[PSA_KEY_DERIVATION_INPUT_SECRET]} *)
  | Input_other_secret
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_OTHER_SECRET}[PSA_KEY_DERIVATION_INPUT_OTHER_SECRET]} *)
  | Input_password
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_PASSWORD}[PSA_KEY_DERIVATION_INPUT_PASSWORD]} *)
  | Input_label
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_LABEL}[PSA_KEY_DERIVATION_INPUT_LABEL]} *)
  | Input_salt
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_SALT}[PSA_KEY_DERIVATION_INPUT_SALT]} *)
  | Input_info
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_INFO}[PSA_KEY_DERIVATION_INPUT_INFO]} *)
  | Input_seed
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_COST}[PSA_KEY_DERIVATION_INPUT_SEED]} *)
  | Input_cost
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_COST}[PSA_KEY_DERIVATION_INPUT_COST]} *)
  (* Not in TF-PSA-Crypto 1.0.0
  | Input_context
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_INPUT_CONTEXT}[PSA_KEY_DERIVATION_INPUT_CONTEXT]} *) *)
  (** The type for
      {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_step_t}
      [psa_key_derivation_step_t]}. *)

  (** {1:functions Functions} *)

  (** Key derivation operations. *)
  module Operation : sig
    type t
    (** The type for {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_operation_t}[psa_key_derivation_operation_t]}.

    {b Note.} These values are finalized by the garbage collector
    via a call to {!Bytesrw_crypto.Psa.Key_derivation.abort}. *)

    val init : unit -> t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_operation_init}[psa_key_derivation_operation_init]} *)
  end

  val setup : Operation.t -> Alg.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_setup}[psa_key_derivation_setup]} *)

  val get_capacity : Operation.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_get_capacity}[psa_key_derivation_get_capacity]} *)

  val set_capacity : Operation.t -> int -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_set_capacity}[psa_key_derivation_set_capacity]} *)

  val input_bytes : Operation.t -> step -> Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_input_bytes}[psa_key_derivation_input_bytes]} *)

  val input_integer : Operation.t -> step -> uint64 -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_input_integer}[psa_key_derivation_input_integer]} *)

  val input_key : Operation.t -> step -> Key_id.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_input_key}[psa_key_derivation_input_key]} *)

  val output_bytes : Operation.t -> Bytes.Slice.t -> Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_output_bytes}[psa_key_derivation_output_bytes]} *)

  val output_key :
    Key_attributes.t -> Operation.t -> (Key_id.t, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_output_key}[psa_key_derivation_output_key]} *)

  val verify_bytes : Operation.t -> Bytes.Slice.t -> Status.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_verify_bytes}[psa_key_derivation_verify_bytes]} *)

  val verify_key : Operation.t -> Key_id.t -> Status.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_verify_key}[psa_key_derivation_verify_key]} *)

  val abort : Operation.t -> Status.t
(** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.psa_key_derivation_abort}[psa_key_derivation_abort]} *)

  (** {1:combine Combining with key agreement} *)

  val key_agreement :
    Operation.t -> step -> private_key:Key_id.t -> peer_key:Bytes.Slice.t ->
    Status.t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.psa_key_derivation_key_agreement}[psa_key_derivation_key_agreement]} *)

  (** {1:support Support functions} *)

  val unlimited_capacity : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_KEY_DERIVATION_UNLIMITED_CAPACITY}[PSA_KEY_DERIVATION_UNLIMITED_CAPACITY]} *)

  val tls12_psk_to_ms_psk_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_TLS12_PSK_TO_MS_PSK_MAX_SIZE}[PSA_TLS12_PSK_TO_MS_PSK_MAX_SIZE]} *)

  (* Not in TF-PSA-Crypto 1.0.0
  val tls12_ecjpake_to_pms_output_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/kdf.html#c.PSA_TLS12_ECJPAKE_TO_PMS_OUTPUT_SIZE}[TLS12_ECJPAKE_TO_PMS_OUTPUT_SIZE]} *) *)
end

(** {2:asymmetric_signature
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html}
    Asymmetric signature}} *)

(** Asymmetric signatures.

    See also {!Alg.asymmetric_signature} in [Alg]. *)
module Sign : sig

  (** {1:functions Functions} *)

  val message :
    Key_id.t -> Alg.t -> input:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.psa_sign_message}[psa_sign_message]} *)

  val verify_message :
    Key_id.t -> Alg.t -> input:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    Status.t
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.psa_verify_message}[psa_verify_message]} *)

  val hash :
    Key_id.t -> Alg.t -> hash:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.psa_sign_hash}[psa_sign_hash]} *)

  val verify_hash :
    Key_id.t -> Alg.t -> input:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    Status.t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.psa_verify_hash}[psa_verify_hash]} *)

  (** {1:support Support functions} *)

  val output_size : Key_type.t -> bits:int -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_SIGN_OUTPUT_SIZE}[PSA_SIGN_OUTPUT_SIZE]} *)

  val signature_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/sign.html#c.PSA_SIGNATURE_MAX_SIZE}[PSA_SIGNATURE_MAX_SIZE]} *)

end

(** {2:asymmetric_encryption
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html}
    Asymmetric encryption}} *)

(** Asymmetric encryption.

    See also {!Alg.asymmetric_encryption} in [Alg]. *)
module Asymmetric : sig

  (** {1:functions Functions} *)

  val encrypt :
    Key_id.t -> Alg.t -> plain:Bytes.Slice.t -> salt:Bytes.Slice.t option ->
    cipher:Bytes.Slice.t -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.psa_asymmetric_encrypt}[psa_asymmetric_encrypt]} *)

  val decrypt :
    Key_id.t -> Alg.t -> cipher:Bytes.Slice.t -> salt:Bytes.Slice.t option ->
    plain:Bytes.Slice.t -> (int, Status.t) result
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.psa_asymmetric_decrypt}[psa_asymmetric_decrypt]} *)

  (** {1:support Support functions} *)

  val encrypt_output_size : Key_type.t -> bits:int -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.PSA_ASYMMETRIC_ENCRYPT_OUTPUT_SIZE}[PSA_ASYMMETRIC_ENCRYPT_OUTPUT_SIZE]} *)

  val encrypt_output_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.PSA_ASYMMETRIC_ENCRYPT_OUTPUT_MAX_SIZE}[PSA_ASYMMETRIC_ENCRYPT_OUTPUT_MAX_SIZE]}*)

  val decrypt_output_size : Key_type.t -> bits:int -> Alg.t -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.PSA_ASYMMETRIC_DECRYPT_OUTPUT_SIZE}[PSA_ASYMMETRIC_DECRYPT_OUTPUT_SIZE]} *)

  val decrypt_output_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/pke.html#c.PSA_ASYMMETRIC_DECRYPT_OUTPUT_MAX_SIZE}[PSA_ASYMMETRIC_DECRYPT_OUTPUT_MAX_SIZE]}*)

end

(** {2:key_agreement
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html}
    Key agreement}} *)

(** Key agreement.

    See also {!Alg.section-key_agreement} in [Alg]. *)
module Key_agreement : sig

  (** {1:functions Functions}

      See also {!Key_derivation.key_agreement}. *)

  val agreement :
    private_key:Key_id.t -> peer_key:Bytes.Slice.t -> Alg.t ->
    Key_attributes.t -> (Key_id.t, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.psa_key_agreement}[psa_key_agreement]} *)

  val raw_agreement :
    Alg.t -> private_key:Key_id.t -> peer_key:Bytes.Slice.t ->
    output:bigbytes -> (int, Status.t) result
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.psa_raw_key_agreement}[psa_raw_key_agreement]} *)

  (** {1:support Support functions} *)

  val raw_output_size : Key_type.t -> bits:int -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_RAW_KEY_AGREEMENT_OUTPUT_SIZE}[PSA_RAW_AGREEMENT_OUTPUT_SIZE]}. *)

  val raw_output_max_size : unit -> int
  (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/ka.html#c.PSA_RAW_KEY_AGREEMENT_OUTPUT_MAX_SIZE}[PSA_RAW_AGREEMENT_OUTPUT_MAX_SIZE]}. *)

end

(** {2:random
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/rng.html#random-number-generation}Random number generation}} *)

val generate_random : Bytes.Slice.t -> Status.t
(** [generate_random s] writes the bytes in the slice range with
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/rng.html#random-number-generation}[psa_generate_random]}. *)

val generate_random_bigbytes : bigbytes -> Status.t
(** [generate_random_bigbytes s] writes the bigbytes bytes with
    {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/rng.html#random-number-generation}[psa_generate_random]}. *)
