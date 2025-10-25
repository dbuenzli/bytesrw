(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [BLAKE3] hashes (via [conf-libblake3])

    This module provides support for the {{:https://blake3.io/}BLAKE3}
    hash with the [libblake3] C library. *)

open Bytesrw

(** The type for [BLAKE3] hashes. *)
module type Blake3 = sig

  (** {1:hashes Hashes} *)

  val id : string
  (** [id] identifies the hash function. *)

  val length : int
  (** [length] is the byte length of hashes produced by the function. *)

  type t
  (** The type for hashes. *)

  type key = t
  (** The type for keys. *)

  (** Hash state. *)
  module State : sig
    type t
    (** The type for hash state. *)

    val make : ?key:key -> unit -> t
    (** [make ?key ()] is an initial hash state with given
        parameters. If [key] is unspecified the hash is unkeyed. *)

    val update : t -> Bytes.Slice.t -> unit
    (** [update state slice] updates [state] with the bytes in the range of
        [slice]. *)
  end

  val value : State.t -> t
  (** [value state] is the hash of [state]. This has no effect on
      [state] which can still be {!State.update}d. *)

  (** {1:hashing Hashing} *)

  val string : ?key:t -> string -> t
  (** [string s] is the hash of [s] keyed with [key] (if any). *)

  val bytes : ?key:t -> bytes -> t
  (** [bytes b] is the hash of [b] keyed with [key] (if any). *)

  val slice : ?key:t -> Bytes.Slice.t -> t
  (** [slice s] is the hash of the bytes in the range of [s] keyed with
      [key] (if any). *)

  val reader : ?key:t -> Bytes.Reader.t -> t
  (** [reader r] is the hash of stream [r] keyed with [key] (if any).
      This consumes the reader. See also {!reads}. *)

  (** {1:streaming Hashing streams} *)

  val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
  (** [reads r] is [hr, hstate] with:
      {ul
      {- [hr] a reader that taps the reads of [r] to update [hstate].}
      {- [hstate], a hash state of the reads made on [hr] so
         far. This is [state] if explicitely given, otherwise
         defaults to a fresh {!State.make}.}}
      To get an intermediate or final hash result use {!value} on
      [hstate]. *)

  val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
  (** [writes ?state w] is [hw, hstate] with:
      {ul
      {- [hw] a writer that taps the writes to update [hstate] before
         giving them to [w].}
      {- [hstate], a hash state of the writes made on [hw] so
         far. This is [state] if explicitely given, otherwise
         defaults to a fresh {!State.make}.}}
      To get an intermediate or final hash result use {!value} on
      [hstate]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is [true] iff [h0] and [h1] are equal. *)

  val compare : t -> t -> int
  (** [comapre] is a total order on hashes compatible with {!equal}. *)

  (** {1:converting Converting} *)

  val to_binary_string : t -> string
  (** [to_binary_string h] is a big-endian binary representation
      of [h] of length {!length}. *)

  val of_binary_string : string -> (t, string) result
  (** [of_binary_string s] is a hash from the big-endian binary
      representation stored in [s]. *)

  val to_hex : t -> string
  (** [to_hex h] is the binary representation of [h] using lowercase
      US-ASCII hex digits. *)

  val of_hex : string -> (t, string) result
  (** [of_hex s] parses a sequence of hex digits into a hash. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats hashes for inspection. *)
end

(** [BLAKE3] hash. *)
module Blake3 : Blake3

(** {1:library Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [blake3] C library. *)
