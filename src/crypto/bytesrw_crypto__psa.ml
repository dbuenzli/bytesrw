(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let strf = Printf.sprintf

type uint8 = int
type uint16 = int
type uint32 = int32
type uint64 = int64
type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let bigbytes_create n = Bigarray.(Array1.create int8_unsigned c_layout n)
let bigbytes_length = Bigarray.Array1.dim
let clear_bytes b = Bytes.fill b 0 (Bytes.length b) '\x00'

(* Note, hard-coded constants are taken from the specifications texts. *)

(* Status codes *)

let success = 0
module Error = struct
  let programmer_error = -129
  let connection_refused = -130
  let connection_busy = -131
  let generic_error = -132
  let not_permitted = -133
  let not_supported = -134
  let invalid_argument = -135
  let invalid_handle = -136
  let bad_state = -137
  let buffer_too_small = -138
  let already_exists = -139
  let does_not_exist = -140
  let insufficient_memory = -141
  let insufficient_storage = -142
  let insufficient_data = -143
  let service_failure = -144
  let communication_failure = -145
  let storage_failure = -146
  let hardware_failure = -147
  let insufficient_entropy = -148
  let invalid_signature = -149
  let invalid_padding = -150
  let corruption_detected = -151
  let data_corrupt = -152
  let data_invalid = -153
end
let operation_incomplete = -248

module Status = struct
  type t = int (* psa_status_t *)
  let equal = Int.equal
  let compare = Int.compare
  let message = function
  | 0 -> "Success"
  | c when c = Error.programmer_error -> strf "Programmer error (%d)" c
  | c when c = Error.connection_refused -> strf "Connection refused (%d)" c
  | c when c = Error.connection_busy -> strf "Connection busy (%d)" c
  | c when c = Error.generic_error -> strf "Generic error (%d)" c
  | c when c = Error.not_permitted -> strf "Not permitted (%d)" c
  | c when c = Error.not_supported -> strf "Not supported (%d)" c
  | c when c = Error.invalid_argument -> strf "Invalid argument (%d)" c
  | c when c = Error.invalid_handle -> strf "Invalid handle (%d)" c
  | c when c = Error.bad_state -> strf "Bad state (%d)" c
  | c when c = Error.buffer_too_small -> strf "Buffer too small (%d)" c
  | c when c = Error.already_exists -> strf "Already exists (%d)" c
  | c when c = Error.does_not_exist -> strf "Does not exist (%d)" c
  | c when c = Error.insufficient_memory -> strf "Insufficent memory (%d)" c
  | c when c = Error.insufficient_storage -> strf "Insufficient storage (%d)" c
  | c when c = Error.insufficient_data -> strf "Insufficient data (%d)" c
  | c when c = Error.service_failure -> strf "Service failure (%d)" c
  | c when c = Error.communication_failure ->
      strf "Communication failure (%d)" c
  | c when c = Error.storage_failure -> strf "Storage failure (%d)" c
  | c when c = Error.hardware_failure -> strf "Hardware failure (%d)" c
  | c when c = Error.invalid_signature -> strf "Invalid signature (%d)" c
  | c when c = Error.corruption_detected -> strf "Corruption detected (%d)" c
  | c when c = Error.data_corrupt -> strf "Data corruption (%d)" c
  | c when c = Error.data_invalid -> strf "Data invalid (%d)" c
  | c -> strf "Unknown error (%d)" c

  let pp ppf st = Format.pp_print_string ppf (message st)

  let bind_success st f = if equal st success then f () else Error st

  module Syntax = struct
    let ( let* ) = Result.bind
    let ( let+ ) = bind_success
  end
end

(* Library management *)

external crypto_api_version : unit -> int * int =
  "ocaml_bytesrw_psa_crypto_api_version"

external crypto_init : unit -> Status.t =
  "ocaml_bytesrw_psa_crypto_init"

(* Algorithms *)

module Alg = struct
  type t = uint32 (* psa_algorithm_t *)
  let none = 0l

  (* Predicates and comparisons *)

  let equal = Int32.equal
  let compare = Int32.unsigned_compare
  let to_uint32 = Fun.id
  let of_uint32 = Fun.id

  (* Categories *)

  external is_hash : t -> bool =
    "ocaml_bytesrw_psa_alg_is_hash"

  external is_mac : t -> bool =
    "ocaml_bytesrw_psa_alg_is_mac"

  external is_cipher : t -> bool =
    "ocaml_bytesrw_psa_alg_is_cipher"

  external is_aead : t -> bool =
    "ocaml_bytesrw_psa_alg_is_aead"

  external is_key_derivation : t -> bool =
    "ocaml_bytesrw_psa_alg_is_key_derivation"

  external is_sign : t -> bool =
    "ocaml_bytesrw_psa_alg_is_sign"

  external is_asymmetric_encryption : t -> bool =
    "ocaml_bytesrw_psa_alg_is_asymmetric_encryption"

  external is_key_agreement : t -> bool =
    "ocaml_bytesrw_psa_alg_is_key_agreement"

  external is_wildcard : t -> bool =
    "ocaml_bytesrw_psa_alg_is_wildcard"

  external get_hash : t -> t =
    "ocaml_bytesrw_psa_alg_get_hash"

  (* Message digest (hashes) *)

  let md2 = 0x02000001l
  let md4 = 0x02000002l
  let md5 = 0x02000003l
  let ripemd160 = 0x02000004l
  let aes_mmo_zigbee = 0x02000007l
  let sha_1 = 0x02000005l
  let sha_224 = 0x02000008l
  let sha_256 = 0x02000009l
  let sha_384 = 0x0200000al
  let sha_512 = 0x0200000bl
  let sha_512_224 = 0x0200000cl
  let sha_512_256 = 0x0200000dl
  let sha3_224 = 0x02000010l
  let sha3_256 = 0x02000011l
  let sha3_384 = 0x02000012l
  let sha3_512 = 0x02000013l
  let shake256_512 = 0x02000015l
  let sm3 = 0x02000014l

  (* Message authentication codes (MAC) *)

  external hmac : t -> t =
    "ocaml_bytesrw_psa_alg_hmac"

  let cbc_mac = 0x03c00100l
  let cbc = 0x03c00200l

  external truncated_mac : t -> length:int -> t =
    "ocaml_bytesrw_psa_alg_truncated_mac"

  external full_length_mac : t -> t =
    "ocaml_bytesrw_psa_alg_full_length_mac"

  external at_least_this_length_mac : t -> length:int -> t =
    "ocaml_bytesrw_psa_alg_at_least_this_length_mac"

  external is_hmac : t -> bool =
    "ocaml_bytesrw_psa_alg_is_hmac"

  external is_block_cipher_mac : t -> bool =
    "ocaml_bytesrw_psa_alg_is_block_cipher_mac"

  (* Unauthenticated ciphers *)

  let stream_cipher = 0x04800100l
  let ctr = 0x04c01000l
  let ccm_star_no_tag = 0x04c01300l
  let ccm_star_any_tag = 0x04c09300l
  let cfb = 0x04c01100l
  let ofb = 0x04c01200l
  let xts = 0x0440ff00l
  let ecb_no_padding = 0x04404400l
  let cbc_no_padding = 0x04404000l
  let cbc_pkcs7 = 0x04404100l

  external is_stream_cipher : t -> bool =
    "ocaml_bytesrw_psa_alg_is_stream_cipher"

  (* Authenticated encryption with associated data (AEAD) *)

  let ccm = 0x05500100l
  let gcm = 0x05500200l
  let chacha20_poly1305 = 0x05100500l
  let xchacha20_poly1305 = 0x05100600l

  external aead_with_shortened_tag : t -> length:int -> t =
    "ocaml_bytesrw_psa_alg_aead_with_shortened_tag"

  external aead_with_default_length_tag : t -> t =
    "ocaml_bytesrw_psa_alg_aead_with_default_length_tag"

  external aead_with_at_least_this_length_tag : t -> length:int -> t =
    "ocaml_bytesrw_psa_alg_aead_with_at_least_this_length_tag"

  external is_aead_on_block_cipher : t -> bool =
    "ocaml_bytesrw_psa_alg_is_aead_on_block_cipher"

  (* Key derivation *)

  external hkdf : t -> t =
    "ocaml_bytesrw_psa_alg_hkdf"

  external hkdf_extract : t -> t =
    "ocaml_bytesrw_psa_alg_hkdf_extract"

  external hkdf_expand : t -> t =
    "ocaml_bytesrw_psa_alg_hkdf_expand"

  (* Not in TF-PSA-Crypto 1.0.0
  external sp800_108_counter_hmac : t -> t =
    "ocaml_bytesrw_psa_alg_sp800_108_counter_hmac" *)

  (* Not in TF-PSA-Crypto 1.0.0
  external sp800_108_counter_cmac : t -> t =
    "ocaml_bytesrw_psa_alg_sp800_108_counter_cmac" *)

  external tls12_prf : t -> t =
    "ocaml_bytesrw_psa_alg_tls12_prf"

  external tls12_psk_to_ms : t -> t =
    "ocaml_bytesrw_psa_alg_tls12_psk_to_ms"

  (* Not in TF-PSA-Crypto 1.0.0
  external tls12_ecjpake_to_pms : t -> t =
    "ocaml_bytesrw_psa_alg_tls12_ecjpake_to_pms" *)

  external pbkdf2_hmac : t -> t =
    "ocaml_bytesrw_psa_alg_pbkdf2_hmac"

  (* Not in TF-PSA-Crypto 1.0.0
  external pbkdf2_aes_cmac_prf_128 : t -> t =
    "ocaml_bytesrw_psa_alg_pbkdf2_aes_cmac_prf_128" *)

  external is_key_derivation_stretching : t -> bool =
    "ocaml_bytesrw_psa_is_key_derivation_stretching"

  external is_hkdf : t -> bool =
    "ocaml_bytesrw_psa_is_hkdf"

  external is_hkdf_extract : t -> bool =
    "ocaml_bytesrw_psa_is_hkdf_extract"

  external is_hkdf_expand : t -> bool =
    "ocaml_bytesrw_psa_is_hkdf_expand"

  (* Not in TF-PSA-Crypto 1.0.0
  external is_sp800_108_counter_hmac : t -> bool =
    "ocaml_bytesrw_psa_is_sp800_108_counter_hmac" *)

  external is_tls12_prf : t -> bool =
    "ocaml_bytesrw_psa_is_tls12_prf"

  (* Not in TF-PSA-Crypto 1.0.0
  external is_tls12_psk_to_ms : t -> bool =
    "ocaml_bytesrw_psa_is_tls12_psk_to_ms" *)

  external is_pbkdf2_hmac : t -> bool =
    "ocaml_bytesrw_psa_is_pbkdf2_hmac"

  (* Asymmetric signature *)

  external rsa_pkcs1v15_sign : t -> t  =
    "ocaml_bytesrw_psa_alg_rsa_pkcs1v15_sign"

  let rsa_pkcs1v15_sign_raw = 0x06000200l

  external rsa_pss : t -> t =
    "ocaml_bytesrw_psa_alg_rsa_pss"

  external rsa_pss_any_salt : t -> t =
    "ocaml_bytesrw_psa_alg_rsa_pss_any_salt"

  external ecdsa : t -> t =
    "ocaml_bytesrw_psa_alg_ecdsa"

  let ecdsa_any = 0x06000600l

  external is_sign_message : t -> bool =
    "ocaml_bytesrw_psa_alg_is_sign_message"

  external is_sign_hash : t -> bool =
    "ocaml_bytesrw_psa_alg_is_sign_hash"

  external is_rsa_pkcs1v15_sign : t -> bool =
    "ocaml_bytesrw_psa_alg_is_rsa_pkcs1v15_sign"

  external is_rsa_pss : t -> bool =
    "ocaml_bytesrw_psa_alg_is_rsa_pss"

  external is_rsa_pss_any_salt : t -> bool =
    "ocaml_bytesrw_psa_alg_is_rsa_pss_any_salt"

  external is_rsa_pss_standard_salt : t -> bool =
    "ocaml_bytesrw_psa_alg_is_rsa_pss_standard_salt"

  external is_ecdsa : t -> bool =
    "ocaml_bytesrw_psa_alg_is_ecdsa"

  external is_deterministic_ecdsa : t -> bool =
    "ocaml_bytesrw_psa_alg_is_deterministic_ecdsa"

  external is_randomized_ecdsa : t -> bool =
    "ocaml_bytesrw_psa_alg_is_randomized_ecdsa"

  external is_hash_eddsa : t -> bool =
    "ocaml_bytesrw_psa_alg_is_hash_eddsa"

  external is_hash_and_sign : t -> bool =
    "ocaml_bytesrw_psa_alg_is_hash_and_sign"

  let any_hash = 0x020000ffl

  external deterministic_ecdsa : t -> t =
    "ocaml_bytesrw_psa_alg_deterministic_ecdsa"

  let pure_eddsa = 0x06000800l
  let ed25519ph = 0x0600090Bl
  let ed448ph = 0x06000915l

  (* Asymmetric encryption *)

  let rsa_pkcs1v15_crypt = 0x07000200l

  external rsa_oaep : t -> t =
    "ocaml_bytesrw_psa_alg_rsa_oaep"

  external is_rsa_oaep : t -> bool =
    "ocaml_bytesrw_psa_alg_is_rsa_oaep"

  (* Key agreement *)

  let ffdh = 0x09010000l
  let ecdh = 0x09020000l

  external key_agreement : ka:t -> kdf:t -> t =
    "ocaml_bytesrw_psa_alg_key_agreement"

  external key_agreement_get_base : t -> t =
    "ocaml_bytesrw_psa_alg_key_agreement_get_base"

  external key_agreement_get_kdf : t -> t =
    "ocaml_bytesrw_psa_alg_key_agreement_get_kdf"

  external is_standalone_key_agreement : t -> bool =
    "ocaml_bytesrw_psa_alg_is_standalone_key_agreement"

  external is_ffdh : t -> bool =
    "ocaml_bytesrw_psa_alg_is_ffdh"

  external is_ecdh : t -> bool =
    "ocaml_bytesrw_psa_alg_is_ecdh"

  (* Formatting *)

  let to_string = function
  | a when equal a none -> "None"
  | a when equal a md2 -> "MD2"
  | a when equal a md4 -> "MD4"
  | a when equal a md5 -> "MD5"
  | a when equal a ripemd160 -> "RIPEMD-160"
  | a when equal a aes_mmo_zigbee -> "MMO_ZIGBEE"
  | a when equal a sha_1 -> "SHA-1"
  | a when equal a sha_224 -> "SHA-224"
  | a when equal a sha_256 -> "SHA-256"
  | a when equal a sha_384 -> "SHA-384"
  | a when equal a sha_512 -> "SHA-512"
  | a when equal a sha_512_224 -> "SHA-512/224"
  | a when equal a sha_512_256 -> "SHA-512/256"
  | a when equal a sha3_224 -> "SHA3-224"
  | a when equal a sha3_256 -> "SHA3-256"
  | a when equal a sha3_384 -> "SHA3-384"
  | a when equal a sha3_512 -> "SHA3-256"
  | a when equal a shake256_512 -> "SHAKE256/512"
  | a when equal a sm3 -> "SM3"
  | a when equal a cbc_mac -> "CBC-MAC"
  | a when equal a cbc -> "CBC"
  | a when equal a ccm -> "CCM"
  | a when equal a gcm -> "GCM"
  | a when equal a chacha20_poly1305 -> "ChaCha20-Poly1305"
  | a when equal a xchacha20_poly1305 -> "xChaCha20-Poly1305"
  | a when equal a rsa_pkcs1v15_sign_raw -> "RSA-PKCS1v15 sign raw"
  | a when equal a ecdsa_any -> "EDCDA any"
  | a when equal a pure_eddsa -> "Pure EDDSA"
  | a when equal a ed25519ph -> "ED25519PH"
  | a when equal a ed448ph -> "ED448PH"
  | a when equal a ffdh -> "FFDH"
  | a when equal a ecdh -> "ECDH"
  (* NB. some of the following outputs can likely be improved.
     By inverting the algorithm constructor of functions. *)
  | a when is_hash a -> strf "hash(0x%lx)" a
  | a when is_mac a -> strf "MAC(0x%lx)" a
  | a when is_cipher a -> strf "cipher(0x%lx)" a
  | a when is_aead a -> strf "AEAD(0x%lx)" a
  | a when is_sign a -> strf "sign(0x%lx)" a
  | a when is_asymmetric_encryption a -> strf "asymmetric(0x%lx)" a
  | a when is_key_agreement a -> strf "key-agreement(0x%lx)" a
  | a when is_key_derivation a -> strf "key-derivation(0x%lx)" a
  | a when is_wildcard a -> strf "wildcard(0x%lx)" a
  | a -> strf "algorithm(0x%lx)" a

  let pp ppf a = Format.pp_print_string ppf (to_string a)
end

(* Key management *)

module Ecc_family = struct
  type t = uint8 (* psa_ecc_family_t *)
  let secp_k1 = 0x17
  let secp_r1 = 0x12
  let secp_r2 = 0x1b
  let sect_k1 = 0x27
  let sect_r1 = 0x22
  let sect_r2 = 0x2b
  let brainpool_p_r1 = 0x30
  let frp = 0x33
  let montgomery = 0x41
  let twisted_edwards = 0x42
  let equal = Int.equal
  let compare = Int.compare
  let to_uint8 = Fun.id
  let of_uint8 = Fun.id
  let to_string = function
  | p when equal p secp_k1 -> "secp*k1"
  | p when equal p secp_r1 -> "secp*r1"
  | p when equal p secp_r2 -> "secp*r2"
  | p when equal p sect_k1 -> "sect*k1"
  | p when equal p sect_r1 -> "sect*r1"
  | p when equal p sect_r2 -> "sect*r2"
  | p when equal p brainpool_p_r1 -> "brainpoolP*r1"
  | p when equal p frp -> "FRP*"
  | p when equal p montgomery -> "Curve*"
  | p when equal p twisted_edwards -> "Edwards*"
  | p -> strf "ecc-family(%x)" p
  let pp ppf p = Format.pp_print_string ppf (to_string p)
end

module Dh_family = struct
  type t = uint8 (* psa_dh_family_t *)
  let rfc7919 = 0x03
  let equal = Int.equal
  let compare = Int.compare
  let to_uint8 = Fun.id
  let of_uint8 = Fun.id
  let to_string = function
  | p when equal p rfc7919 -> "RFC7919"
  | p -> strf "dh-family(%x)" p
  let pp ppf p = Format.pp_print_string ppf (to_string p)
end

module Key_id = struct
  type t = uint32 (* psa_key_id_t *)
  let null = 0l
  let user_min = 0x00000001l
  let user_max = 0x3fffffffl
  let vendor_min = 0x40000000l
  let vendor_max = 0x7fffffffl
  let equal = Int32.equal
  let compare = Int32.unsigned_compare
  let to_uint32 = Fun.id
  let of_uint32 = Fun.id
  let pp ppf i = Format.fprintf ppf "key-id(%lx)" i
end

module Key_location = struct
  type t = uint32 (* psa_key_location_t *)
  let local_storage = 0x000000l
  let primary_secure_element = 0x000001l
  let equal = Int32.equal
  let compare = Int32.unsigned_compare
  let to_uint32 = Fun.id
  let of_uint32 = Fun.id
  let to_string = function
  | l when equal l local_storage -> "local-storage"
  | l when equal l primary_secure_element -> "primary-secure-element"
  | l -> strf "key-location(%lx)" l
  let pp ppf l = Format.pp_print_string ppf (to_string l)
end

module Key_persistence = struct
  type t = uint8 (* psa_key_persistence_t *)
  let volatile = 0x00
  let default = 0x01
  let read_only = 0xff
  let equal = Int.equal
  let compare = Int.compare
  let to_uint8 = Fun.id
  let of_uint8 = Fun.id
  let to_string = function
  | p when equal p volatile -> "volatile"
  | p when equal p default -> "default"
  | p when equal p read_only -> "read-only"
  | p -> strf "key-persistence(%x)" p
  let pp ppf p = Format.pp_print_string ppf (to_string p)
end

module Key_lifetime = struct
  type t = uint32 (* psa_key_lifetime_t *)
  let volatile   = 0x00000000l
  let persistent = 0x00000001l

  let from_persistence_and_location p l =
    Int32.(logand (shift_left l 8) (of_int p))

  let get_persistence l = Int32.to_int (Int32.logand l 0xFFl)
  let get_location l = Int32.shift_right_logical l 8
  let is_volatile l =
    Key_persistence.equal (get_persistence l) Key_persistence.volatile

  let equal = Int32.equal
  let compare = Int32.unsigned_compare
  let to_uint32 = Fun.id
  let of_uint32 = Fun.id
  let pp ppf i = Format.fprintf ppf "key-lifetime(%lx)" i
end

module Key_usage = struct
  type t = uint32 (* psa_key_usage_t *)
  let ( + ) = Int32.logor
  let ( - ) u u' = Int32.(logand u (lognot u'))
  let test ~has u = Int32.(unsigned_compare (logand has u) has = 0)
  let none = 0l
  let export = 0x00000001l
  let copy = 0x00000002l
  let cache = 0x00000004l
  let encrypt = 0x00000100l
  let decrypt = 0x00000200l
  let sign_message = 0x00000400l
  let verify_message = 0x00000800l
  let sign_hash = 0x00001000l
  let verify_hash = 0x00002000l
  let derive = 0x00004000l
  let verify_derivation = 0x00008000l
  let equal = Int32.equal
  let compare = Int32.unsigned_compare
  let to_uint32 = Fun.id
  let of_uint32 = Fun.id
  let to_string = function
  | p -> strf "key-usage(%lx)" p
  let pp ppf p = Format.pp_print_string ppf (to_string p)
end

module Key_type = struct
  type t = uint16 (* psa_key_type_t *)

  let none = 0x0000

  (* Key categories *)

  external is_unstructured : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_unstructured"

  external is_asymmetric : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_asymmetric"

  external is_public_key : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_public_key"

  external is_key_pair : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_key_pair"

  (* Symmetric keys *)

  let raw_data = 0x1001
  let hmac = 0x1100
  let derive = 0x1200
  let password = 0x1203
  let password_hash = 0x1205
  let pepper = 0x1206
  let aes = 0x2400
  let aria = 0x2406
  let des = 0x2301
  let camellia = 0x2403
  let sm4 = 0x2405
  let arc4 = 0x2002
  let chacha20 = 0x2004
  let xchacha20 = 0x2007

  (* RSA keys *)

  let rsa_key_pair = 0x7001
  let rsa_public_key = 0x4001
  external is_rsa : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_rsa"

  (* ECC keys *)

  external ecc_key_pair : Ecc_family.t -> t =
    "ocaml_bytesrw_psa_key_type_ecc_key_pair"

  external ecc_public_key : Ecc_family.t -> t =
    "ocaml_bytesrw_psa_key_type_ecc_public_key"

  external is_ecc : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_ecc"

  external is_ecc_key_pair : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_ecc_key_pair"

  external is_ecc_public_key : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_ecc_public_key"

  external ecc_get_family : t -> Ecc_family.t =
    "ocaml_bytesrw_psa_key_type_ecc_get_family"

  (* Diffie-Hellman *)

  external dh_key_pair : Dh_family.t -> t =
    "ocaml_bytesrw_psa_key_type_dh_key_pair"

  external dh_public_key : Dh_family.t -> t =
    "ocaml_bytesrw_psa_key_type_dh_public_key"

  external is_dh : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_dh"

  external is_dh_key_pair : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_dh_key_pair"

  external is_dh_public_key : t -> bool =
    "ocaml_bytesrw_psa_key_type_is_dh_public_key"

  external dh_get_family : t -> Dh_family.t =
    "ocaml_bytesrw_psa_key_type_dh_get_family"

  (* Generic public keys *)

  external key_pair_of_public_key : t -> t =
    "ocaml_bytesrw_psa_key_type_key_pair_of_public_key"

  external public_key_of_key_pair : t -> t =
    "ocaml_bytesrw_psa_key_type_public_key_of_key_pair"

  (* Predicates and comparisons *)

  let equal = Int.equal
  let compare = Int.compare

  (* Converting *)

  let to_uint16 = Fun.id
  let of_uint16 = Fun.id
  let to_string = function
  | t when equal t raw_data -> "raw-data"
  | t when equal t hmac -> "hmac"
  | t when equal t derive -> "derive"
  | t when equal t password -> "password"
  | t when equal t password_hash -> "password-hash"
  | t when equal t pepper -> "pepper"
  | t when equal t aes -> "aes"
  | t when equal t aria -> "aria"
  | t when equal t des -> "des"
  | t when equal t camellia -> "camellia"
  | t when equal t sm4 -> "sm4"
  | t when equal t arc4 -> "arc4"
  | t when equal t chacha20 -> "chacha20"
  | t when equal t xchacha20 -> "xchacha20"
  | t when equal t rsa_key_pair ->"rsa-key-pair"
  | t when equal t rsa_public_key -> "rsa-public-key"
  | t when is_ecc_public_key t ->
      strf "ecc-public-key-%s" (Ecc_family.to_string (ecc_get_family t))
  | t when is_ecc_key_pair t ->
      strf "ecc-public-key-pair-%s" (Ecc_family.to_string (ecc_get_family t))
  | t when is_dh_public_key t ->
      strf "dh-public-key-%s" (Dh_family.to_string (dh_get_family t))
  | t when is_ecc_key_pair t ->
      strf "dh-public-key-pair-%s" (Dh_family.to_string (dh_get_family t))
  | t when is_unstructured t -> strf "unstructured(%x)" t
  | t when is_asymmetric t -> strf "asymmetric(%x)" t
  | t when is_public_key t -> strf "public-key(%x)" t
  | t when is_key_pair t -> strf "key-pair(%x)" t
  | t -> strf "key-type(%x)" t
  let pp ppf t = Format.pp_print_string ppf (to_string t)
end

(* Key attributes *)

module Key_attributes = struct
  type t (* Custom value with a pointer to a psa_key_attributes_t,
            finalized by a call to psa_reset_key_attributes.  *)

  external init : unit -> t =
    "ocaml_bytesrw_psa_key_attributes_init"

  external get_key_attributes : Key_id.t -> t -> Status.t =
    "ocaml_bytesrw_psa_get_key_attributes"

  external reset_key_attributes : t -> unit =
    "ocaml_bytesrw_psa_reset_key_attributes"

  external get_key_algorithm : t -> Alg.t =
    "ocaml_bytesrw_psa_get_key_algorithm"

  external set_key_algorithm : t -> Alg.t -> unit =
    "ocaml_bytesrw_psa_set_key_algorithm"

  external get_key_bits : t -> int =
    "ocaml_bytesrw_psa_get_key_bits"

  external set_key_bits : t -> int -> unit =
    "ocaml_bytesrw_psa_set_key_bits"

  external get_key_id : t -> Key_id.t =
    "ocaml_bytesrw_psa_get_key_id"

  external set_key_id : t -> Key_id.t -> unit =
    "ocaml_bytesrw_psa_set_key_id"

  external get_key_lifetime : t -> Key_lifetime.t =
    "ocaml_bytesrw_psa_get_key_lifetime"

  external set_key_lifetime : t -> Key_lifetime.t -> unit =
    "ocaml_bytesrw_psa_set_key_lifetime"

  external get_key_type : t -> Key_type.t =
    "ocaml_bytesrw_psa_get_key_type"

  external set_key_type : t -> Key_type.t -> unit =
    "ocaml_bytesrw_psa_set_key_type"

  external get_key_usage_flags : t -> Key_usage.t =
    "ocaml_bytesrw_psa_get_key_usage_flags"

  external set_key_usage_flags : t -> Key_usage.t -> unit =
    "ocaml_bytesrw_psa_set_key_usage_flags"

  let pp ppf a =
    Format.fprintf ppf
      "@[<v>@[algorithm = %a@]@,@[bits = %d@]@,@[id = %a@]@,\
       @[lifetime = %a@]@,@[type = %a@]@,@[usage = %a@]@]"
      Alg.pp (get_key_algorithm a) (get_key_bits a) Key_id.pp (get_key_id a)
      Key_lifetime.pp (get_key_lifetime a) Key_type.pp (get_key_type a)
      Key_usage.pp (get_key_usage_flags a)
end

let get_key_attributes = Key_attributes.get_key_attributes
let reset_key_attributes = Key_attributes.reset_key_attributes
let get_key_algorithm = Key_attributes.get_key_algorithm
let set_key_algorithm = Key_attributes.set_key_algorithm
let get_key_bits = Key_attributes.get_key_bits
let set_key_bits = Key_attributes.set_key_bits
let get_key_id = Key_attributes.get_key_id
let set_key_id = Key_attributes.set_key_id
let get_key_lifetime = Key_attributes.get_key_lifetime
let set_key_lifetime = Key_attributes.set_key_lifetime
let get_key_type = Key_attributes.get_key_type
let set_key_type = Key_attributes.set_key_type
let get_key_usage_flags = Key_attributes.get_key_usage_flags
let set_key_usage_flags = Key_attributes.set_key_usage_flags

(* Key creation *)

external unsafe_import_key :
  Key_attributes.t -> bigbytes -> length:int -> (Key_id.t, Status.t) result =
  "ocaml_bytesrw_psa_import_key"

let import_key ?length ka bb =
  let bb_len = bigbytes_length bb in
  let length = match length with
  | None -> bb_len
  | Some length ->
      if 0 <= length && length <= bb_len then length else
      invalid_arg (strf "length not in range [0;%d]" bb_len)
  in
  unsafe_import_key ka bb ~length

external generate_key : Key_attributes.t -> (Key_id.t, Status.t) result =
  "ocaml_bytesrw_psa_generate_key"

external copy_key : Key_id.t -> Key_attributes.t -> (Key_id.t, Status.t) result=
  "ocaml_bytesrw_psa_copy_key"

(* Key destruction *)

external destroy_key : Key_id.t -> Status.t =
  "ocaml_bytesrw_psa_destroy_key"

external purge_key : Key_id.t -> Status.t =
  "ocaml_bytesrw_psa_purge_key"

(* Key export *)

external export_key : Key_id.t -> bigbytes -> (int, Status.t) result =
  "ocaml_bytesrw_psa_export_key"

external export_public_key : Key_id.t -> bigbytes -> (int, Status.t) result =
  "ocaml_bytesrw_psa_export_public_key"

external export_key_output_size : Key_type.t -> bits:int -> int =
  "ocaml_bytesrw_psa_export_key_output_size"

external export_public_key_output_size : Key_type.t -> bits:int -> int =
  "ocaml_bytesrw_psa_export_public_key_output_size"

external export_key_pair_max_size : unit -> int =
  "ocaml_bytesrw_psa_export_key_pair_max_size"

external export_public_key_max_size : unit -> int =
  "ocaml_bytesrw_psa_export_public_key_max_size"

(* Message digest *)

module Hash = struct
  (* Single-part *)

  external compute :
    Alg.t -> input:Bytes.Slice.t -> hash:Bytes.Slice.t -> (int, Status.t) result
    =
    "ocaml_bytesrw_psa_hash_compute"

  external compare :
    Alg.t -> input:Bytes.Slice.t -> hash:Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_hash_compare"

  (* Multi-part *)

  module Operation = struct
    type t
    (* Custom value with a pointer to a psa_hash_operation_t,
       finalized by a call to psa_hash_abort.  *)

    external init : unit -> t =
      "ocaml_bytesrw_psa_hash_operation_init"
  end

  external setup : Operation.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_hash_setup"

  external update : Operation.t -> Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_hash_update"

  external finish :
    Operation.t -> hash:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_hash_finish"

  external verify : Operation.t -> hash:Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_hash_verify"

  external abort : Operation.t -> Status.t =
    "ocaml_bytesrw_psa_hash_abort"

  (* Not in TF-PSA-Crypto 1.0.0
  external suspend :
    Operation.t -> state:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_hash_suspend" *)

  (* Not in TF-PSA-Crypto 1.0.0
  external resume : Operation.t -> state:Bytes.Slice.t -> Status.t =
     "ocaml_bytesrw_psa_hash_resume" *)

  external clone : src:Operation.t -> dst:Operation.t -> Status.t =
    "ocaml_bytesrw_psa_hash_clone"

  external length : Alg.t -> int =
    "ocaml_bytesrw_psa_hash_length"

  external max_size : unit -> int =
    "ocaml_bytesrw_psa_hash_max_size"

  (* Not in TF-PSA-Crypto 1.0.0
  external suspend_output_size : Alg.t -> int =
    "ocaml_bytesrw_psa_hash_suspend_output_size" *)

  (* Not in TF-PSA-Crypto 1.0.0
  external suspend_output_max_size : unit -> int =
    "ocaml_bytesrw_psa_hash_suspend_output_max_size" *)

  (* Not in TF-PSA-Crypto 1.0.0
  external suspend_algorithm_field_length : unit -> int =
    "ocaml_bytesrw_psa_hash_suspend_algorithm_field_length" *)

  (* Not in TF-PSA-Crypto 1.0.0
  external suspend_input_length_field_length : Alg.t -> int =
    "ocaml_bytesrw_psa_hash_suspend_input_length_field_length" *)

  (* Not in TF-PSA-Crypto 1.0.0
  external suspend_input_length_field_length : Alg.t -> int =
    "ocaml_bytesrw_psa_hash_suspend_hash_state_field_length" *)

  external block_length : Alg.t -> int =
    "ocaml_bytesrw_psa_hash_block_length"
end

module Mac = struct
  external length : Key_type.t -> bits:int -> Alg.t -> int =
    "ocaml_bytesrw_psa_mac_length"

  external max_size : unit -> int =
    "ocaml_bytesrw_psa_mac_max_size"

  external compute :
    key:Key_id.t -> Alg.t -> input:Bytes.Slice.t -> mac:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_mac_compute"

  external verify :
    key:Key_id.t -> Alg.t -> input:Bytes.Slice.t -> mac:Bytes.Slice.t ->
    Status.t =
    "ocaml_bytesrw_psa_mac_verify"

  module Operation = struct
    type t
    (* Custom value with a pointer to a psa_mac_operation_t,
       finalized by a call to psa_mac_abort.  *)

    external init : unit -> t =
      "ocaml_bytesrw_psa_mac_operation_init"
  end

  external sign_setup : Operation.t -> Key_id.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_mac_sign_setup"

  external verify_setup : Operation.t -> Key_id.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_mac_verify_setup"

  external update : Operation.t -> Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_mac_update"

  external sign_finish :
    Operation.t -> hash:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_mac_sign_finish"

  external verify_finish : Operation.t -> hash:Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_mac_verify_finish"

  external abort : Operation.t -> Status.t =
    "ocaml_bytesrw_psa_mac_abort"
end

module Cipher = struct
  external encrypt :
    key:Key_id.t -> Alg.t -> plain:Bytes.Slice.t -> cipher:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_cipher_encrypt"

  external decrypt :
    key:Key_id.t -> Alg.t -> cipher:Bytes.Slice.t -> plain:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_cipher_decrypt"

  module Operation = struct
    type t
    (* Custom value with a pointer to a psa_cipher_operation_t,
       finalized by a call to psa_cipher_abort.  *)

    external init : unit -> t =
      "ocaml_bytesrw_psa_cipher_operation_init"
  end

  external encrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_cipher_encrypt_setup"

  external decrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_cipher_decrypt_setup"

  external generate_iv :
    Operation.t -> iv:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_cipher_generate_iv"

  external set_iv : Operation.t -> iv:Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_cipher_set_iv"

  external update :
    Operation.t -> input:Bytes.Slice.t -> output:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_cipher_update"

  external finish :
    Operation.t -> output:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_cipher_finish"

  external abort : Operation.t -> Status.t =
    "ocaml_bytesrw_psa_cipher_abort"

    external encrypt_output_size :
    Key_type.t -> Alg.t -> plain_length:int -> int =
    "ocaml_bytesrw_psa_cipher_encrypt_output_size"

  external encrypt_output_max_size : plain_length:int -> int =
    "ocaml_bytesrw_psa_cipher_encrypt_output_max_size"

  external decrypt_output_size :
    Key_type.t -> Alg.t -> cipher_length:int -> int =
    "ocaml_bytesrw_psa_cipher_decrypt_output_size"

  external decrypt_output_max_size : cipher_length:int -> int =
    "ocaml_bytesrw_psa_cipher_decrypt_output_max_size"

  external iv_length : Key_type.t -> Alg.t -> int =
    "ocaml_bytesrw_psa_cipher_iv_length"

  external iv_max_size : unit -> int =
    "ocaml_bytesrw_psa_cipher_iv_max_size"

  external update_output_size :
    Key_type.t -> Alg.t -> input_length:int -> int =
    "ocaml_bytesrw_psa_cipher_update_output_size"

  external update_output_max_size : input_length:int -> int =
    "ocaml_bytesrw_psa_cipher_update_output_max_size"

  external finish_output_size : Key_type.t -> Alg.t -> int =
    "ocaml_bytesrw_psa_cipher_finish_output_size"

  external finish_output_max_size : unit -> int =
    "ocaml_bytesrw_psa_cipher_finish_output_max_size"

  external block_length : Key_type.t -> int =
    "ocaml_bytesrw_psa_block_cipher_block_length"

  external block_max_size : unit -> int =
    "ocaml_bytesrw_psa_block_cipher_block_max_size"
end

module Aead = struct
  external encrypt :
    key:Key_id.t -> Alg.t -> nonce:Bytes.Slice.t -> ad:Bytes.Slice.t ->
    plain:Bytes.Slice.t -> cipher:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_aead_encrypt_bc" "ocaml_bytesrw_psa_aead_encrypt"

  external decrypt :
    key:Key_id.t -> Alg.t -> nonce:Bytes.Slice.t -> ad:Bytes.Slice.t ->
    cipher:Bytes.Slice.t -> plain:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_aead_encrypt_bc" "ocaml_bytesrw_psa_aead_decrypt"

  module Operation = struct
    type t
    (* Custom value with a pointer to a psa_aead_operation_t,
       finalized by a call to psa_aead_abort.  *)

    external init : unit -> t =
      "ocaml_bytesrw_psa_aead_operation_init"
  end

  external encrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_aead_encrypt_setup"

  external decrypt_setup : Operation.t -> key:Key_id.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_aead_decrypt_setup"

  external set_lengths :
    Operation.t -> ad_length:int -> plain_length:int -> Status.t =
    "ocaml_bytesrw_psa_aead_set_lengths"

  external generate_nonce :
    Operation.t -> nonce:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_aead_generate_nonce"

  external set_nonce : Operation.t -> nonce:Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_aead_set_nonce"

  external update_ad : Operation.t -> ad:Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_aead_update_ad"

  external update :
    Operation.t -> input:Bytes.Slice.t -> output:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_aead_update"

  external finish :
    Operation.t -> cipher:Bytes.Slice.t -> tag:Bytes.Slice.t ->
    (int * int, Status.t) result =
    "ocaml_bytesrw_psa_aead_finish"

  external verify :
    Operation.t -> plain:Bytes.Slice.t -> tag:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_aead_verify"

  external abort : Operation.t -> Status.t =
    "ocaml_bytesrw_psa_aead_abort"

  external encrypt_output_size :
    Key_type.t -> Alg.t -> plain_length:int -> int =
    "ocaml_bytesrw_psa_aead_encrypt_output_size"

  external encrypt_output_max_size : plain_length:int -> int =
    "ocaml_bytesrw_psa_aead_encrypt_output_max_size"

  external decrypt_output_size :
    Key_type.t -> Alg.t -> cipher_length:int -> int =
    "ocaml_bytesrw_psa_aead_decrypt_output_size"

  external decrypt_output_max_size : cipher_length:int -> int =
    "ocaml_bytesrw_psa_aead_decrypt_output_max_size"

  external nonce_length : Key_type.t -> Alg.t -> int =
    "ocaml_bytesrw_psa_aead_nonce_length"

  external nonce_max_size : unit -> int =
    "ocaml_bytesrw_psa_aead_nonce_max_size"

  external update_output_size :
    Key_type.t -> Alg.t -> input_length:int -> int =
    "ocaml_bytesrw_psa_aead_update_output_size"

  external update_output_max_size : input_length:int -> int =
    "ocaml_bytesrw_psa_aead_update_output_max_size"

  external finish_output_size : Key_type.t -> Alg.t -> int =
    "ocaml_bytesrw_psa_aead_finish_output_size"

  external finish_output_max_size : unit -> int =
    "ocaml_bytesrw_psa_aead_finish_output_max_size"

  external tag_length : Key_type.t -> bits:int -> Alg.t -> int =
    "ocaml_bytesrw_psa_aead_tag_length"

  external tag_max_size : unit -> int =
    "ocaml_bytesrw_psa_aead_tag_max_size"

  external verify_output_size : Key_type.t -> Alg.t -> int =
    "ocaml_bytesrw_psa_aead_verify_output_size"

  external verify_output_max_size : unit -> int =
    "ocaml_bytesrw_psa_aead_verify_output_max_size"
end

module Key_derivation = struct

  type step =
  (* keep in sync with [ocaml_bytesrw_psa_derivation_step] table in C stub *)
  | Input_secret
  | Input_other_secret
  | Input_password
  | Input_label
  | Input_salt
  | Input_info
  | Input_seed
  | Input_cost
  (* Not in TF-PSA-Crypto 1.0.0 | Input_context *)

  module Operation = struct
    type t
    (* Custom value with a pointer to a psa_key_derivation_operation_t,
       finalized by a call to psa_key_derivation_abort.  *)

    external init : unit -> t =
      "ocaml_bytesrw_psa_key_derivation_operation_init"
  end

  external setup : Operation.t -> Alg.t -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_setup"

  external get_capacity : Operation.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_key_derivation_get_capacity"

  external set_capacity : Operation.t -> int -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_set_capacity"

  external input_bytes : Operation.t -> step -> Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_input_bytes"

  external input_integer : Operation.t -> step -> uint64 -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_input_integer"

  external input_key : Operation.t -> step -> Key_id.t -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_input_key"

  external output_bytes : Operation.t -> Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_output_bytes"

  external output_key :
    Key_attributes.t -> Operation.t -> (Key_id.t, Status.t) result =
    "ocaml_bytesrw_psa_key_derivation_output_key"

  external verify_bytes : Operation.t -> Bytes.Slice.t -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_verify_bytes"

  external verify_key : Operation.t -> Key_id.t -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_verify_key"

  external abort : Operation.t -> Status.t =
    "ocaml_bytesrw_psa_key_derivation_abort"

  external key_agreement :
    Operation.t -> step -> private_key:Key_id.t -> peer_key:Bytes.Slice.t ->
    Status.t =
    "ocaml_bytesrw_psa_key_derivation_key_agreement"

  external unlimited_capacity : unit -> int =
    "ocaml_bytesrw_psa_key_derivation_unlimited_capacity"

  external tls12_psk_to_ms_psk_max_size : unit -> int =
    "ocaml_bytesrw_psa_tls12_psk_to_ms_psk_max_size"

  (* Not in TF-PSA-Crypto 1.0.0
  external tls12_ecjpake_to_pms_output_size : unit -> int =
     "ocaml_bytesrw_psa_tls12_ecjpake_to_pms_output_size" *)
end

(* Asymmetric signatures *)

module Sign = struct
  external message :
    Key_id.t -> Alg.t -> input:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_sign_message"

  external verify_message :
    Key_id.t -> Alg.t -> input:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    Status.t =
    "ocaml_bytesrw_psa_verify_message"

  external hash :
    Key_id.t -> Alg.t -> hash:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    (int, Status.t) result =
    "ocaml_bytesrw_psa_sign_hash"

  external verify_hash :
    Key_id.t -> Alg.t -> input:Bytes.Slice.t -> signature:Bytes.Slice.t ->
    Status.t =
    "ocaml_bytesrw_psa_verify_hash"

  external output_size : Key_type.t -> bits:int -> Alg.t -> int =
    "ocaml_bytesrw_psa_sign_output_size"

  external signature_max_size : unit -> int =
    "ocaml_bytesrw_psa_signature_max_size"
end

(* Asymmetric encryption *)

module Asymmetric = struct
  external encrypt :
    Key_id.t -> Alg.t -> plain:Bytes.Slice.t -> salt:Bytes.Slice.t option ->
    cipher:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_asymmetric_encrypt"

  external decrypt :
    Key_id.t -> Alg.t -> cipher:Bytes.Slice.t -> salt:Bytes.Slice.t option ->
    plain:Bytes.Slice.t -> (int, Status.t) result =
    "ocaml_bytesrw_psa_asymmetric_decrypt"

  external encrypt_output_size : Key_type.t -> bits:int -> Alg.t -> int =
    "ocaml_bytesrw_psa_asymmetric_encrypt_output_size"

  external encrypt_output_max_size : unit -> int =
    "ocaml_bytesrw_psa_asymmetric_encrypt_output_max_size"

  external decrypt_output_size : Key_type.t -> bits:int -> Alg.t -> int =
    "ocaml_bytesrw_psa_asymmetric_decrypt_output_size"

  external decrypt_output_max_size : unit -> int =
    "ocaml_bytesrw_psa_asymmetric_decrypt_output_max_size"
end

(* Key agreement *)

module Key_agreement = struct
  external agreement :
    private_key:Key_id.t -> peer_key:Bytes.Slice.t -> Alg.t ->
    Key_attributes.t -> (Key_id.t, Status.t) result =
    "ocaml_bytesrw_psa_key_agreement"

  external raw_agreement :
    Alg.t -> private_key:Key_id.t -> peer_key:Bytes.Slice.t ->
    output:bigbytes -> (int, Status.t) result =
    "ocaml_bytesrw_psa_raw_key_agreement"

  external raw_output_size : Key_type.t -> bits:int -> int =
    "ocaml_bytesrw_psa_raw_key_agreement_output_size"

  external raw_output_max_size : unit -> int =
    "ocaml_bytesrw_psa_raw_key_agreement_output_max_size"
end

(* Random number generation *)

external generate_random : Bytes.Slice.t -> Status.t =
  "ocaml_bytesrw_psa_generate_random"

external generate_random_bigbytes : bigbytes -> Status.t =
  "ocaml_bytesrw_psa_generate_random_bigbytes"
