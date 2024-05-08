(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [XXH3-64] and [XXH3-128] stream hashes.

    This module provides support for the {{:https://xxhash.com/}XXH3}
    hash family with the [libxxhash] C library. *)

open Bytesrw

(** The type for [XXH3] hash functions. *)
module type Xxh3 = sig

  (** {1:hashes Hashes} *)

  val id : string
  (** [id] identifies the hash function. *)

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

        Raises [Invalid_argument] if your secret is smaller than
        {!Bytesrw_xxhash.xxh3_secret_size_min}. *)

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

  val reader : ?seed:seed -> Bytes.Reader.t -> t
  (** [reader r] hashes the stream of [r] with seed [seed] (if any).
      See also {!reads}. *)

  (** {1:streaming Hashing streams} *)

  val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
  (** [reads r] is [hr, hstate] with:
      {ul
      {- [hr] a reader that taps the reads of [r] to update [hstate].}
      {- [hstate], a hash state of the reads made on [hr] so
         far. This is [state] if explicitely given, otherwise
         defaults to a fresh {!State.make}.}}
      To get intermediate or final hash results use {!value} on
      [hstate]. *)

  val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
  (** [writes ?state w] is [hw, hstate] with:
      {ul
      {- [hw] a writer that taps the writes to update [hstate] before
         giving them to [w].}
      {- [hstate], a hash state of the writes made on [wr] so
         far. This is [state] if explicitely given, otherwise
         defaults to a fresh {!State.make}.}}
      To get intermediate or final hash results use {!value} on
      [hstate]. *)

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

(** [XXH3-64] hash. *)
module Xxh3_64 : sig
  include Xxh3 (** @inline *)

  val to_uint64 : t -> int64
  (** [to_int64 h] is [h] as an unsigned [int64] number. *)

  val of_uint64 : int64 -> t
  (** [of_uint64 u] is [u] as a hash. *)
end


(** [XXH3-128] hash. *)
module Xxh3_128 : Xxh3

(** {1:library Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [xxhash] C library. *)

val xxh3_secret_size_min : unit -> int
(** [xxh3_secret_size_min ()] is the bare minimum size for a custom
    secret. *)
