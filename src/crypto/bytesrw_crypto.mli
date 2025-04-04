(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cryptographic primitives and streams (via [conf-mbedtls])

    {b Note.} The initialisation code of this module calls
    {!Psa.crypto_init} and raises a {!Panic} exception if a problem
    occurs. Your program cannot catch this raise but it should
    probably not run when that happens.

    {b Primitive availability.} The available cryptographic primitives
    are provided by a
    {{:https://arm-software.github.io/psa-api/crypto/1.2/}PSA Crypto
    API} implementation. Not all algorithms mentioned in the
    specification are necessarily available. For now this binds the
    {{:https://github.com/Mbed-TLS/TF-PSA-Crypto}TF-PSA-Crypto}
    implementation of
    {{:https://www.trustedfirmware.org/projects/mbed-tls/}Mbed TLS}.
    However the binding code should be usable with any implementation
    of the API. *)

open Bytesrw

(** {1:errors Errors} *)

exception Panic of string
(** Except for the low-level {!Psa} module, any function from this
    module may raise this exception. It is not meant to be handled,
    let it flow at the toplevel to print it and abort your program or
    server request. Panics occur for two reasons:
    {ul
    {- Systemic reasons. Something is really wrong in the system. For
       example there is a lack of entropy.}
    {- Programming errors made by users of the high level interface.
       These programming error may trigger {!Psa.Error.bad_state}
       in the low-level {!Psa} implementation. For example using
       {!Hash.value} twice on a state. These errors
       are the moral equivalent of raising [Invalid_argument] but
       not converted as such because {!Psa.Error.bad_state} is not one-to-one
       with such conditions.}} *)

type Bytes.Stream.error +=
| Error of string (** *)
(** The type for crypto streams errors.

    Readers and writers using cryptographic primitives may raise
    {!Bytes.Bytes.Stream.Error} with this error. *)

(** {1:preliminaries Preliminaries} *)

type uint8 = int
(** The type for unsigned 8-bit integers. *)

type uint16 = int
(** The type for unsigned 16-bit integers. *)

type uint32 = int32
(** The type for unsigned 32-bit integers. *)

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** The type bigarrays of bytes. *)

(** Bigbytes operations. *)
module Bigbytes : sig

  type t = bigbytes
  (** The type for bigbytes. *)

  val create : int -> bigbytes
  (** [create n] are [n] uninitialized bytes. *)

  val make : int -> fill:uint8 -> bigbytes
  (** [make n ~fill] is bigbytes of length [n] initalized with byte [fill]. *)

  val init : int -> (int -> uint8) -> t
  (** [init n init] is a bigbytes of length [n] with element [i] initialized
      by [init i]. *)

  val length : bigbytes -> int
  (** [length b] is the length of [b]. *)

  val copy : bigbytes -> bigbytes
  (** [copy b] is a copy of [b]. *)

  (** {1:conv Converting} *)

  val of_string : string -> bigbytes
  (** [of_string s] is a bigbytes value with the contents of [s]. *)

  val to_string : ?length:int -> bigbytes -> string
  (** [to_string b] is a string with the first [length] bytes of [b].
      [length] defaults to [length b]. *)

  val of_bytes : bytes -> bigbytes
  (** [of_bytes s] is a bigbytes value with the contents of [s]. *)

  val to_bytes : ?length:int -> bigbytes -> bytes
  (** [to_bytes b] is a bytes value with the first [length] bytes of [b].
      [length] defaults to [length b]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats bigbytes for inspection. *)
end

(** Zeroing mutable bytes. *)
module Clear : sig
  val slice : Bytes.Slice.t -> unit
  (** [slice s] sets the bytes in the range of [s] to ['\x00']. *)

  val bytes : bytes -> unit
  (** [bytes b] sets the bytes of [b] to ['\x00']. *)

  val bigbytes : bigbytes -> unit
  (** [bigbytes b] sets the bytes of [b] to ['\x00']. *)
end

(** Constant time byte equality checking.

    {b Note.} The functions raise [Invalid_argument] on sequence length
    mismatch or empty sequences. In the context where these functions should
    be used, these conditions usually means something is not done right. *)
module Verify : sig
  val equal_slices : Bytes.Slice.t -> Bytes.Slice.t -> bool
  (** [equal_slices s0 s1] checks in constant time that the bytes in
      the range of [s0] and [s1] are equal. Raises [Invalid_argument]
      if the length of slices are not positive and equal. *)

  val equal_strings : string -> string -> bool
  (** [equal_strings s0 s1] checks in constant time that the bytes of
      [s0] and [s1] are equal. Raises [Invalid_argument] if the length
      of strings are not positive and equal. *)

  val equal_bytes : bytes -> bytes -> bool
  (** [equal_bytes b0 b1] checks in constant time that the bytes of
      [b1] and [b2] are equal. Raises [Invalid_argument] if the length
      of bytes are not positive and equal. *)

  val equal_bigbytes : bigbytes -> bigbytes -> bool
  (** [equal_bigbytes b0 b1] checks in constant time that the bytes of [b1] and
      [b2] are equal. Raises [Invalid_argument] if the length of bigbytes
      are not positive and equal. *)
end

(** {1:crypto Cryptography} *)

(** {2:hashes Hashes} *)

(** Generic and dedicated message digests (hashes).

    {b Warning.} Before using a hash algorithm, check it is
    {!Hash.Algorithm.is_supported} otherwise {{!Bytesrw_crypto.Panic}panics}
    are raised on usage.

    {b Dedicated modules.} If you need to manipulate hash values of a
    specific algorithm beyond a simple check it's better to use a
    {{!Hash.dedicated_modules} dedicated module}.  By giving you a
    proper type for its hash values it will make it for clearer and
    avoid possible mixups; For example here is a dedicated module
    for algorithm [Sha_384]:
{[
module Sha_384 = Bytesrw_crypto.Hash.Make
   (struct let algorithm = Bytesrw_crypto.Hash.Algorithm.Sha_384 end)
]}
    A few {{!Bytesrw_crypto.hashes}predefined hash modules} are provided
    for hashes in widespread use in 2025. *)
module Hash : sig

  (** {1:algorithms Hash algorithms} *)

  (** Hash algorithm identifiers. *)
  module Algorithm : sig

    (** {1:algorithm_ids Algorithm identifiers} *)

    type t =
    | Aes_mmo_zigbee
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/about.html#cite-zigbee}[AES-MMO-ZIGBEE]} *)
    | Md2 (** {{:https://datatracker.ietf.org/doc/html/rfc1319.html}[MD2]} {b WARNING} weak, only use for legacy applications. *)
    | Md4 (** {{:https://datatracker.ietf.org/doc/html/rfc1320}[MD4]} {b WARNING} weak, only use for legacy applications. *)
    | Md5 (** {{:https://datatracker.ietf.org/doc/html/rfc1321}[MD5]} {b WARNING} weak, only use for legacy applications. *)
    | Ripemd_160 (** {{:https://homes.esat.kuleuven.be/~bosselae/ripemd160.html}[RIPEMD-160]} *)
    | Sha3_224 (** {{:https://doi.org/10.6028/NIST.FIPS.202}[SHA3-224]} *)
    | Sha3_256 (** {{:https://doi.org/10.6028/NIST.FIPS.202}[SHA3-256]} *)
    | Sha3_384 (** {{:https://doi.org/10.6028/NIST.FIPS.202}[SHA3-384]} *)
    | Sha3_512 (** {{:https://doi.org/10.6028/NIST.FIPS.202}[SHA3-512]} *)
    | Sha_1 (** {{:https://doi.org/10.6028/NIST.FIPS.180-4}[SHA-1]} {b WARNING} weak, only use for legacy applications. *)
    | Sha_224 (** {{:https://doi.org/10.6028/NIST.FIPS.180-4}[SHA-223]} *)
    | Sha_256 (** {{:https://doi.org/10.6028/NIST.FIPS.180-4}[SHA-256]} *)
    | Sha_384 (** {{:https://doi.org/10.6028/NIST.FIPS.180-4}[SHA-384]} *)
    | Sha_512 (** {{:https://doi.org/10.6028/NIST.FIPS.180-4}[SHA-512]} *)
    | Sha_512_224 (** {{:https://doi.org/10.6028/NIST.FIPS.180-4}[SHA-512-224]} *)
    | Sha_512_256 (** {{:https://doi.org/10.6028/NIST.FIPS.180-4}[SHA-512-256]} *)
    | Shake256_512 (** {{:https://dx.doi.org/10.6028/NIST.FIPS.202}[SHAKE-256-512]} *)
    | Sm3 (** {{:https://www.iso.org/standard/67116.html}[SM3]} *)
    (** The type for hash algorithm identifiers.

        Use {!is_supported} to see which ones are supported by the
        current implementation. *)

    val is_supported : t -> bool
    (** [is_supported a] is [true] iff the current implementation
        supports the hash algorithm [a]. *)

    val length : t -> int
    (** [length a] is the length in bytes of hashes of algorithm [a].
        Note that this can be [0] if the algorithm is
        {{!is_supported}unsupported}. *)

    (** {1:predicates Predicates and comparisons} *)

    val equal : t -> t -> bool
    (** [equal] tests algorithm identifiers for equality. *)

    val compare : t -> t -> int
    (** [compare] is a total order compatible with {!equal}. *)

    (** {1:converting Converting} *)

    val of_string : string -> (t, string) result
    (** [of_string s] is an algorithm from [s]. Lowercased
        match of [s] against the identifiers specified in
        {{!t}these comments}. *)

    val to_string : t -> string
    (** [to_string a] is an ASCII string for [a]. Lowercased
        identifiers mentioned in {{!t}these comments}. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats algorithms for inspection. *)
  end

  (** {1:hashes Hashes} *)

  type t
  (** The type for generic hashes. These hashes are not tied to an
      algorithm. Values of this type just represent the result of any
      hash algorithm as a sequence of bytes. Use {{!dedicated_modules}
      a dedicated module} to work with a particular type hash. *)

  val length : t -> int
  (** [length h] is the length of hash [h]. See also {!Algorithm.length}. *)

  (** Hash state. *)
  module State : sig

    type t
    (** The type for hash states. *)

    val make : Algorithm.t -> t
    (** [make ()] is an initial state for hash algorithm [alg]. *)

    val update : t -> Bytes.Slice.t -> unit
    (** [update state slice] updates [state] with the bytes in the range of
        [slice]. *)

    val copy : t -> t
    (** [copy state] is a copy of [state], operations on [state] do
        not affect the copy and vice-versa. {b Warning.} Trying
        to copy a {!state} whose {!value} has been *)

    (* Once we have tfpsasupport for Hash.{suspend,resume}

    val to_binary_string : t -> string
    (** [to_binary_string state] is a binary representation of [state]. *)

    val of_binary_string : string -> (t, string) result
    (** [of_binary_state s] is a state from a binary representation
       [s] gotten from {!to_binary_string}. *)
    *)
  end

  val value : State.t -> t
  (** [value state] is the hash of [state].

      {b Warning.} This must be called only {b once}. It has an effect
      on [state]. Trying to do the following operations afterwards
      raises {!Bytesrw_crypto.Panic} exceptions:
      {ul
      {- Using {!State.update} or {!State.copy}. Make a {!State.copy} before
         if you want to get an intermediate hash.}
      {- Calling again {!value} or {!verify_value} on [state].}} *)

  val verify_value : State.t -> t -> bool
  (** [verify_value state h] checks in constant time that [value
      state] is equal to [h].

      {b Warning.} This has all the caveats of {!value}. *)

  (** {1:hashing Hashing} *)

  val string : Algorithm.t -> string -> t
  (** [string alg s] is the [alg] hash of [s]. *)

  val bytes : Algorithm.t -> bytes -> t
  (** [bytes alg b] is the [alg] hash of [b]. *)

  val slice : Algorithm.t -> Bytes.Slice.t -> t
  (** [slice alg s] is the [alg] hash of the bytes in the range of [s]. *)

  val reader : Algorithm.t -> Bytes.Reader.t -> t
  (** [reader alg r] is the [alg] hash of stream [r]. This consumes the reader.
      See also {!reads}. *)

  (** {1:streaming Hashing streams} *)

  val reads : State.t -> Bytes.Reader.t -> Bytes.Reader.t
  (** [reads state r] is [hr] with:
      {ul
      {- [hr] a reader that taps the reads of [r] to update [state].}
      {- [state], the hash state to udpate.}}
      To get the final hash result use {!value} on [state] {b once}. *)

  val writes : State.t -> Bytes.Writer.t -> Bytes.Writer.t
  (** [writes state w] is [hw] with:
      {ul
      {- [hw] a writer that taps the writes to update [state] before
         giving them to [w].}
      {- [state], the hash state to udpate.}}
      To get the final hash result use {!value} on [state] {b once}. *)

  (** {1:predicates Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] uses {!Verify.equal_strings} to assert the equality
      of [h0] and [h1]. Raises [Invalid_argument] if the hashes length
      is not positive and equal. *)

  (** {1:converting Converting} *)

  val to_binary_string : t -> string
  (** [to_binary_string h] is a binary representation
      of [h] of length {!length}. *)

  val of_binary_string : ?length:int -> string -> (t, string) result
  (** [of_binary_string s] is a hash from binary representation stored
      in [s]. If [length] is specified, errors if the hash is not
      [length] bytes. *)

  val to_hex : t -> string
  (** [to_hex h] is the binary representation of [h] using lowercase
      US-ASCII hex digits. *)

  val of_hex : ?length:int -> string -> (t, string) result
  (** [of_hex s] parses a sequence of hex digits into a hash.
      If [length] is specified, errors if the resulting hash has not [length]
      bytes. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats hashes for inspection. *)

  (** {1:dedicated_modules Dedicated modules} *)

  (** The type for dedicated hash modules. *)
  module type T = sig

    (** {1:algorithm Algorithm} *)

    val algorithm : Algorithm.t
    (** [algorithm] is the hash algorithm. *)

    val id : string
    (** [id] identifies the algorithm. This is
        {!Algorithm.to_string}[ algorithm]. *)

    val length : int
    (** [length] is the byte length of hashes produced by the
        function. Note that this can be [0] if the algorithm is
        {{!Bytesrw_crypto.Hash.Algorithm.is_supported}unsupported}. *)

    (** {1:hashes Hashes} *)

    type t
    (** The type for hashes. *)

    (** Hash state. *)
    module State : sig
      type t
      (** The type for hash state. *)

      val make : unit -> t
      (** [make ()] is an initial hash state. *)

      val update : t -> Bytes.Slice.t -> unit
      (** [update state slice] updates [state] with the bytes in the range of
          [slice]. *)

      val copy : t -> t
      (** [copy state] is a copy of [state], operations on [state] do
          not affect the copy. *)

      (* Once we have tfpsacrypto support for Hash.{suspend,resume} *)
        (*
       val to_binary_string : t -> string
       val of_binary_string : string -> t
      *)
    end

    val value : State.t -> t
    (** [value state] is the hash of [state].

        {b Warning.} This must be called only {b once}. It has an effect
        on [state]. Trying to do the following operations afterwards
        raises {!Bytesrw_crypto.Panic} exceptions:
        {ul
        {- Using {!State.update} or {!State.copy}. Make a {!State.copy} before
           if you want to get an intermediate hash.}
        {- Calling again {!value} or {!verify_value} on [state].}} *)

    val verify_value : State.t -> t -> bool
    (** [verify_value state h] checks in constant time that [value
        state] is equal to [h].

        {b Warning.} This has all the caveats of {!value}. *)

    (** {1:hashing Hashing} *)

    val string : string -> t
    (** [string s] is the hash of [s]. *)

    val bytes : bytes -> t
    (** [bytes b] is the hash of [b]. *)

    val slice : Bytes.Slice.t -> t
    (** [slice s] is the hash of the bytes in the range of [s]. *)

    val reader : Bytes.Reader.t -> t
    (** [reader r] is the hash of stream [r]. This consumes the reader.
        See also {!reads}. *)

    (** {1:streaming Hashing streams} *)

    val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
    (** [reads r] is [hr, hstate] with:
        {ul
        {- [hr] a reader that taps the reads of [r] to update [hstate].}
        {- [hstate], a hash state of the reads made on [hr] so
           far. This is [state] if explicitely given, otherwise
           defaults to a fresh {!State.make}.}}
        To get the final hash result use {!value} on [hstate] {b once}. *)

    val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
    (** [writes ?state w] is [hw, hstate] with:
        {ul
        {- [hw] a writer that taps the writes to update [hstate] before
           giving them to [w].}
        {- [hstate], a hash state of the writes made on [hw] so
           far. This is [state] if explicitely given, otherwise
           defaults to a fresh {!State.make}.}}
        To get the final hash result use {!value} on [hstate] {b once}. *)

    (** {1:preds Predicates and comparisons} *)

    val equal : t -> t -> bool
    (** [equal h0 h1] uses {!Verify.equal_strings} to assert the
        equality of [h0] and [h1]. *)

    (** {1:converting Converting} *)

    val to_binary_string : t -> string
    (** [to_binary_string h] is a binary representation of [h] of
        length {!length}. *)

    val of_binary_string : string -> (t, string) result
    (** [of_binary_string s] is a hash from the binary representation
        stored in [s]. *)

    val to_hex : t -> string
    (** [to_hex h] is the binary representation of [h] using lowercase
        US-ASCII hex digits. *)

    val of_hex : string -> (t, string) result
    (** [of_hex s] parses a sequence of hex digits into a hash. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats hashes for inspection. *)
  end

  (** The type for hash algorithm specification. *)
  module type ALGORITHM = sig
    val algorithm : Algorithm.t
    (** The hash algorithm. *)
  end

  (** [Make (A)] is a hash module for the hashes of the algorithm specifed
      by [A]. *)
  module Make (Algorithm : ALGORITHM) : T
end

(** [SHA-256] hashes. *)
module Sha_256 : Hash.T

(** [SHA-512] hashes. *)
module Sha_512 : Hash.T

(*

(** {2:mac MAC} *)

(** Generic and dedicated message authentication codes (MAC). *)
module Mac : sig

  (** {1:algorithms MAC algorithms} *)

  (** MAC algorithm identifiers. *)
  module Algorithm : sig

    (** {1:algorithm_ids Algorithm identifiers} *)

    type t =
    | Hmac of Hash.Algorithm.t
    (** {{:https://datatracker.ietf.org/doc/html/rfc2104.html}HMAC}
        over given hash algorithm. *)
    | Cbc_mac
    (** {{:https://www.iso.org/standard/50375.html}CBC-MAC}
        {b WARNING} Insecure in many cases, defined by the key type.  *)
    | Cmac
    (** {{:https://www.iso.org/standard/50375.html}CBC-MAC},
        defined by the key type. *)
    | Truncated of t * int
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_TRUNCATED_MAC}truncated MAC}. *)
    | Full_length of t
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_FULL_LENGTH_MAC}full length MAC} *)
    | At_least_length of t * int
    (** {{:https://arm-software.github.io/psa-api/crypto/1.2/api/ops/macs.html#c.PSA_ALG_AT_LEAST_THIS_LENGTH_MAC}at least length MAC}. *)
    (** The type for MAC algorithm identifiers.

        Use {!is_supported} to see which ones are supported by the
        current implementation. *)

    (*

    val is_supported : t -> bool
    (** [is_supported a] is [true] iff the current implementation
        supports the hash algorithm [a]. *)

    val length : t -> int
    (** [length a] is the length in bytes of hashes of algorithm [a].
        Note that this can be [0] if the algorithm is
        {{!is_supported}unsupported}. *)

    (** {1:predicates Predicates and comparisons} *)

    val equal : t -> t -> bool
    (** [equal] tests algorithm identifiers for equality. *)

    val compare : t -> t -> int
    (** [compare] is a total order compatible with {!equal}. *)

    (** {1:converting Converting} *)

    val of_string : string -> (t, string) result
    (** [of_string s] is an algorithm from [s]. Lowercased
        match of [s] against the identifiers specified in
        {{!t}these comments}. *)

    val to_string : t -> string
    (** [to_string a] is an ASCII string for [a]. Lowercased
        identifiers mentioned in {{!t}these comments}. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats algorithms for inspection. *)
       *)
  end

  (** {1:MACs MACs} *)

  type key
  (** The type for HMAC keys. *)

  type t
  (** The type for generic MACs. These MACs are not tied to an
      algorithm. Values of this type just represent the result of any
      MAC algorithm as a sequence of bytes. *)

  val length : t -> int
  (** [length mac] is the length of hash [mac]. See also {!Algorithm.length}. *)

  (** MAC state. *)
  module State : sig

    type t
    (** The type for MAC states. *)

    val make : key:key -> Algorithm.t -> t
    (** [make ()] is an initial MAC state for MAC algorithm [alg]. *)

    val update : t -> Bytes.Slice.t -> unit
    (** [update state slice] updates [state] with the bytes in the range of
        [slice]. *)
  end

  val value : State.t -> t
  (** [value state] is the hash of [state].

      {b Warning.} This must be called only {b once}. It has an effect
      on [state]. Trying to do the following operations afterwards
      raises {!Bytesrw_crypto.Panic} exceptions:
      {ul
      {- Using {!State.update} or {!State.copy}. Make a {!State.copy} before
         if you want to get an intermediate hash.}
      {- Calling again {!value} on [state].}} *)

  val verify_value : State.t -> t -> bool
  (** TODO add that to hash too *)

  (** {1:macing MACing} *)

  val string : key:key -> Algorithm.t -> string -> t
  (** [string alg s] is the [alg] hash of [s]. *)

  val bytes : key:key -> Algorithm.t -> bytes -> t
  (** [bytes alg b] is the [alg] hash of [b]. *)

  val slice : key:key -> Algorithm.t -> Bytes.Slice.t -> t
  (** [slice alg s] is the [alg] hash of the bytes in the range of [s]. *)

  val reader : key:key -> Algorithm.t -> Bytes.Reader.t -> t
  (** [reader alg r] is the [alg] hash of stream [r]. This consumes the reader.
      See also {!reads}. *)

  (** {1:streaming MACing streams} *)

  val reads : State.t -> Bytes.Reader.t -> Bytes.Reader.t
  (** [reads state r] is [hr] with:
      {ul
      {- [hr] a reader that taps the reads of [r] to update [state].}
      {- [state], the hash state to udpate.}}
      To get the final hash result use {!value} on [state] {b once}. *)

  val writes : State.t -> Bytes.Writer.t -> Bytes.Writer.t
  (** [writes state w] is [hw] with:
      {ul
      {- [hw] a writer that taps the writes to update [state] before
         giving them to [w].}
      {- [state], the MAC state to update.}}
      To get the final MAC result use {!value} on [state] {b once}. *)

  (** {1:predicates Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal m0 m1] uses {!Verify.equal_strings} to assert the equality
      of [m0] and [m1]. Raises [Invalid_argument] if the MACs length
      is not positive and equal. *)

  (** {1:converting Converting} *)

  val to_binary_string : t -> string
  (** [to_binary_string m] is a binary representation
      of [m] of length {!length}. *)

  val of_binary_string : ?length:int -> string -> (t, string) result
  (** [of_binary_string s] is a MAC from binary representation stored
      in [s]. If [length] is specified, errors if the MAC is not
      [length] bytes. *)

  val to_hex : t -> string
  (** [to_hex m] is the binary representation of [m] using lowercase
      US-ASCII hex digits. *)

  val of_hex : ?length:int -> string -> (t, string) result
  (** [of_hex s] parses a sequence of hex digits into a MAC.
      If [length] is specified, errors if the resulting MAC has not [length]
      bytes. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats hashes for inspection. *)
end
*)

(** {2:random Randomness} *)

(** Cryptographically secure pseudorandom byte streams.

    This module provides cryptographically secure pseudorandom
    bytes. See the documentation of the {{!Random.primitive}primitive}
    for details on the source.

    {b Note.} If you are only interested in this bit from the library you may
    want to use {!Bytesrw_sysrandom} which depends only on your operating
    system. *)
module Random : sig

  (** {1:csprng Cryptographically secure pseurorandom bytes} *)

  val reads :
    ?pos:Bytes.Stream.pos -> ?slice_length:Bytes.Slice.length -> ?length:int ->
    unit -> Bytes.Reader.t
  (** [reads ()] is a stream of [length], or unbounded if unspecified,
      cryptographically secure pseudorandom bytes in [slice_length]
      chunks (defaults to {!Bytesrw.Bytes.Slice.default_length}).

      Reads may raise {!Panic} which is not turned into a stream
      error as it likely indicates a serious condition in the system,
      see the underlying call {!set_random}. *)

  val string : int -> string
  (** [string n] are [n] cryptographically secure pseudorandom
      bytes. Raises {!Panic} in case of problems, see the underlying
      call {!set_random}. *)

  val bytes : int -> bytes
  (** [bytes n] are [n] cryptographically secure pseudorandom
      bytes. Raises {!Panic} in case of problems,
      see the underlying call {!set_random}. *)

  (** {1:primitive Primitive} *)

  val set_random : Bytes.Slice.t -> unit
  (** [set_random s] writes the bytes in the slice range with
      cryptographically secure pseudorandom bytes.

      This uses:

      {ul
      {- {!Psa.generate_random}}}

      Raises {!Panic} if {!Psa.generate_random} errors. If this
      happens do not try to handle the exception, log it at the
      toplevel of your program and abort the program or the server
      request. It likely indicates a serious error condition in the
      system. *)
end

(** {1:low Low-level cryptography}

    This is a low-level interface. If the service can be found in the
    high-level interface, favour it. *)

module Psa = Bytesrw_crypto__psa
