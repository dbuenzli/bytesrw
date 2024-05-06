(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [XXH3-{64,128}] stream hashes.

    This module provides support for the {{:https://xxhash.com/}XXH3}
    hash family with the [libxxhash] C library. *)

open Bytesrw

(** XXH3-64 hash *)
module Xxh3_64 : sig

  (** {1:hashes Hashes} *)

  val id : string
  (** [id] is ["xxh3-64"] to identify the hash function. *)

  val length : int
  (** [length] is the byte length of hashes produced by the function. *)

  type seed = int64
  (** The type for seeds. *)

  type secret = string
  (** The type for secrets. *)

  type t
  (** The type for hashes. *)

  (** Hashing state. *)
  module State : sig
    type t
    (** The type for hashing state. *)

    val make : ?secret:secret -> ?seed:seed -> unit -> t
    (** [make ?secret ?seed ()] is an initial hashing state with given
        parameters. If unspecified the hash is unseeded and there is
        no secret.

        Raises [Invalid_argument]
        if your secret is smaller than {!Bytesrw_xxh.xhh3_secret_min_size}. *)

    val update : t -> Bytes.Slice.t -> unit
    (** [update state slice] updates [state] with [slice]. *)

    val copy : t -> t
    (** [copy t] is a copy of [t]. *)
  end

  val value : State.t -> t
  (** [value state] is the hash of [state]. This has no effect on
      [state] which can still be {!State.update}d. *)

  (** {1:hashing Hashing} *)

  val string : ?seed:seed -> string -> t
  (** [string s] is the hash of [s] with seed [seed] (if any). *)

  val bytes : ?seed:seed -> bytes -> t
  (** [bytes b] is the hash of [b] with seed [seed] (if any). *)

  val slice : ?seed:seed -> Bytes.Slice.t -> t
  (** [slice s] is the hash of [s] with seed [seed] (if any). *)

  (** {1:streaming Hashing streams} *)

  val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
  (** [reads r] is [hr, hstate] with:
      {ul
      {- [hr] a reader returning the reads of [r] unaltered
         and updating [state].}
      {- [hstate], a hash state of the reads made on [hr] so
         far. This is [state] if explicitely given, otherwise
         defaults to a fresh {!State.make}.}} *)

  val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
  (** [writes ?state r] is a writer hashing the writes of [w]. Use
      {!finish} at any time for the hash of [w]'s writes so far. If
      [state] is provided it is the returned state used for the
      updates. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is [true] iff [h0] and [h1] are equal. *)

  val compare : t -> t -> int
  (** [comapre] is a total order on hashes compatible with {!equal}. *)

  (** {1:converting Converting} *)

  val to_binary_string : t -> string
  (** [to_binary_string s] is a big-endian binary representation
      of [s] of length {!length}. *)

  val of_binary_string : string -> (t, string) result
  (** [of_binary_string s] is a hash from the big-endian binary
      representation stored in [s]. *)

  val to_hex : t -> string
  (** [to_hex t] is the binary representation of [h] using lowercase
      US-ASCII hex digits. *)

  val of_hex : string -> (t, string) result
  (** [to_hex t] parses a sequence of hex digits into a hash. *)

  val to_uint64 : t -> int64
  (** [to_int64 h] is [h] as an unsigned [int64] number. *)

  val of_uint64 : int64 -> t
  (** [of_uint64 u] is [u] as a hash. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats hashes for inspection. *)
end

(** XXH3-128 hash *)
module Xxh3_128 : sig

  (** {1:hashes Hashes} *)

  val id : string
  (** [id] is ["xxh3-128"] to identify the hash function. *)

  val length : int
  (** [length] is the byte length of hashes produced by the function. *)

  type secret = string
  (** The type for secrets. *)

  type seed = int64
  (** The type for seeds. *)

  type t
  (** The type for hashes. *)

  (** Hashing state. *)
  module State : sig
    type t
    (** The type for hashing state. *)

    val make : ?secret:secret -> ?seed:seed -> unit -> t
    (** [make ?secret ?seed ()] is an initial hashing state with given
        parameters. If unspecified the hash is unseeded and there is
        no secret.

        Raises [Invalid_argument]
        if your secret is smaller than {!Bytesrw_xxh.xhh3_secret_min_size}. *)

    val update : t -> Bytes.Slice.t -> unit
    (** [update state slice] updates [state] with [slice]. *)

    val copy : t -> t
    (** [copy t] is a copy of [t]. *)
  end

  val value : State.t -> t
  (** [value state] is the hash of [state]. This has no effect on
      [state] which can still be {!State.update}d. *)

  (** {1:hashing Hashing} *)

  val string : ?seed:seed -> string -> t
  (** [string s] is the hash of [s] with seed [seed] (if any). *)

  val bytes : ?seed:seed -> bytes -> t
  (** [bytes b] is the hash of [b] with seed [seed] (if any). *)

  val slice : ?seed:seed -> Bytes.Slice.t -> t
  (** [slice s] is the hash of [s] with seed [seed] (if any). *)

  (** {1:streaming Hashing streams} *)

  val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
  (** [reads r] is [hr, hstate] with:
      {ul
      {- [hr] a reader returning the reads of [r] unaltered
         and updating [state].}
      {- [hstate], a hash state of the reads made on [hr] so
         far. This is [state] if explicitely given, otherwise
         defaults to a fresh {!State.make}.}} *)

  val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
  (** [writes ?state r] is a writer hashing the writes of [w]. Use
      {!finish} at any time for the hash of [w]'s writes so far. If
      [state] is provided it is the returned state used for the
      updates. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is [true] iff [h0] and [h1] are equal. *)

  val compare : t -> t -> int
  (** [comapre] is a total order on hashes compatible with {!equal}. *)

  (** {1:converting Converting} *)

  val to_binary_string : t -> string
  (** [to_binary_string s] is a big-endian binary representation
      of [s] of length {!length}. *)

  val of_binary_string : string -> (t, string) result
  (** [of_binary_string s] is a hash from the big-endian binary
      representation stored in [s]. *)

  val to_hex : t -> string
  (** [to_hex t] is the binary representation of [h] using lowercase
      US-ASCII hex digits. *)

  val of_hex : string -> (t, string) result
  (** [to_hex t] parses a sequence of hex digits into a hash. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats hashes for inspection. *)
end

(** {1:library Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [xxhash] C library. *)

val xxh3_secret_size_min : unit -> int
(** [xxh3_secret_size_min ()] is the bare minimum size for a custom
    secret. *)
