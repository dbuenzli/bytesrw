(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [XXH] family stream hashes.

    This module provides support for the {{:https://xxhash.com/}XXH}
    hash family. *)

open Bytesrw


(** XXH3-64 hash *)
module Xxh3_64 : sig

  (** {1:hashes Hashes} *)

  val id : string
  (** [id] is an US-ASCII string identifying the hash function. *)

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

    val make : ?seed:seed -> ?secret:secret -> unit -> t
    (** [make ?seed ?secret ()] is a hashing state with given
        parameters. *)

    val update : t -> Bytes.Slice.t -> unit
    (** [update state slice] updates [state] with [slice]. *)

    val copy : t -> t
    (** [copy t] is a copy of [t]. *)
  end

  val value : State.t -> t
  (** [value state] is the XXH64 hash of [state]. This has no effect on
      [state] which can still be {!update}d. *)

  (** {1:hashing Hashing} *)

  val string : ?seed:seed -> string -> t
  (** [string s] is the hash of [s]. *)

  val bytes : ?seed:seed -> bytes -> t
  (** [bytes b] is the hash of [b]. *)

  val slice : ?seed:seed -> Bytes.Slice.t -> t
  (** [slice s] is the hash of [s]. *)

  (** {1:streaming Hashing streams} *)

  val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
  (** [reads ?state r] is a reader hashing the reads of [r] and hash
      state. Use {!value} on the hash state for the hash of [r]'s
      reads so far. [state] defaults to a fresh {!State.make}[
      ()]. Note that {!Bytesrw.Bytes.Reader.push_back} do not affect
      the hash.  [state] is the state used, it defaults to a fresh
      {!State.make}[ ()]. *)

  val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
  (** [writes ?state r] is a writer hashing the writes of [w]. Use {!finish}
      at any time for the hash of [w]'s writes so far. If [state]
      is provided it is the returned state used for the updates. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal h0 h1] is [true] iff [h0] and [h1] are equal. *)

  val compare : t -> t -> int
  (** [comapre] is a total order on hashes compatible with {!equal}. *)

  (** {1:converting Converting} *)

  val to_binary_string : t -> string
  val to_int64 : t -> int64
end


(** {1:library Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [xxhash] C library. *)
