(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* Library parameters *)

external version : unit -> string = "ocaml_bytesrw_blake3_version"

(* The type for blake3 hashes *)

module type Blake3 = sig
  val id : string
  val length : int
  type t
  type key = t
  module State : sig
    type t
    val make : ?key:key -> unit -> t
    val update : t -> Bytes.Slice.t -> unit
  end
  val value : State.t -> t
  val string : ?key:t -> string -> t
  val bytes : ?key:t -> bytes -> t
  val slice : ?key:t -> Bytes.Slice.t -> t
  val reader : ?key:t -> Bytes.Reader.t -> t
  val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
  val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_binary_string : t -> string
  val of_binary_string : string -> (t, string) result
  val to_hex : t -> string
  val of_hex : string -> (t, string) result
  val pp : Format.formatter -> t -> unit
end

(* BLAKE3 hash *)

module Blake3_hasher = struct
  type t  (* Custom value holding a blake3_hasher struct *)
  type hash = string (* 32 bytes *)
  type key = hash
  external create : unit -> t = "ocaml_bytesrw_blake3_create"
  external init : t -> unit = "ocaml_bytesrw_blake3_init"
  external init_keyed : t -> key:key -> unit = "ocaml_bytesrw_blake3_init_keyed"
  external finalize : t -> hash = "ocaml_bytesrw_blake3_finalize"
  external update : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_blake3_update"

  external hash : bytes -> int -> int -> hash = "ocaml_bytesrw_blake3_hash"
  external hash_keyed : key:key -> bytes -> int -> int -> hash =
    "ocaml_bytesrw_blake3_hash_keyed"
end

module Blake3 = struct
  module State = struct
    type t = Blake3_hasher.t

    let make ?key () =
      let state = Blake3_hasher.create () in
      begin match key with
      | None -> Blake3_hasher.init state
      | Some key -> Blake3_hasher.init_keyed state ~key
      end;
      state

    let update state s =
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and last = Bytes.Slice.length s in
      Blake3_hasher.update state b first last
  end

  let id = "blake3"
  let length = 32
  type t = Blake3_hasher.hash
  type key = t

  let value = Blake3_hasher.finalize
  let bytes ?key b = match key with
  | None -> Blake3_hasher.hash b 0 (Bytes.length b)
  | Some key -> Blake3_hasher.hash_keyed ~key b 0 (Bytes.length b)

  let string ?key s = bytes ?key (Bytes.unsafe_of_string s)

  let slice ?key s =
    let b = Bytes.Slice.bytes s in
    let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
    match key with
    | None -> Blake3_hasher.hash b first length
    | Some key -> Blake3_hasher.hash_keyed ~key b first length

  let reader ?key r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ?key ()) r

  let reads ?(state = State.make ()) r =
    Bytes.Reader.tap (State.update state) r, state

  let writes ?(state = State.make ()) w =
    Bytes.Writer.tap (State.update state) w, state

  let equal = String.equal
  let compare = String.compare
  let of_binary_string s = Bytesrw_hex.check_binary_string_length ~length s
  let to_binary_string = Fun.id
  let of_hex s = Bytesrw_hex.to_binary_string ~length s
  let pp = Bytesrw_hex.pp_binary_string
  let to_hex = Bytesrw_hex.of_binary_string
end
