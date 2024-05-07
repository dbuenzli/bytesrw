(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* XXX find a scheme to avoid these code dupes with xxh and blake3 *)

let strf = Printf.sprintf

let binary_string_of_hex h = (* raises Failure *)
  let hex_value s i = match s.[i] with
  | '0' .. '9' as c -> Char.code c - 0x30
  | 'A' .. 'F' as c -> 10 + (Char.code c - 0x41)
  | 'a' .. 'f' as c -> 10 + (Char.code c - 0x61)
  | c -> failwith (strf "%d: %C is not an ASCII hexadecimal digit" i c)
  in
  match String.length h with
  | len when len mod 2 <> 0 -> failwith "Missing final hex digit"
  | len ->
      let rec loop max s i h k = match i > max with
      | true -> Bytes.unsafe_to_string s
      | false ->
          let hi = hex_value h k and lo = hex_value h (k + 1) in
          Bytes.set s i (Char.chr @@ (hi lsl 4) lor lo);
          loop max s (i + 1) h (k + 2)
      in
      let s_len = len / 2 in
      let s = Bytes.create s_len in
      loop (s_len - 1) s 0 h 0

let of_hex ~length of_binary_string s =
  let slen = String.length s in
  if slen = length * 2
  then match binary_string_of_hex s with
  | exception Failure e -> Error e
  | h -> of_binary_string h
  else
  Error
    (strf
       "Expected %d ASCII hexadecimal digit found %d characters" length slen)

let pp_hex ppf s =
  for i = 0 to String.length s - 1 do
    Format.fprintf ppf "%02x" (Char.code (s.[i]))
  done

let string_to_hex s = Format.asprintf "%a" pp_hex s

let of_binary_string ~length s =
  let slen = String.length s in
  if slen = length
  then Ok s else Error (strf  "Expected %d bytes, found %d" length slen)

(* SHA hash signature *)

module type Sha = sig
  val id : string
  val length : int
  type t
  module State : sig
    type t
    val make : unit -> t
    val update : t -> Bytes.Slice.t -> unit
  end
  val value : State.t -> t
  val string : string -> t
  val bytes : bytes -> t
  val slice : Bytes.Slice.t -> t
  val reader : Bytes.Reader.t -> t
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

(* SHA-1 *)

module Sha1_ctx = struct
  type t  (* Custom value whose data holds a SHA1_CTX C struct *)
  type hash = string (* 20 bytes *)
  external hash : bytes -> int -> int -> hash = "ocaml_bytesrw_sha1_hash"
  external init : unit -> t = "ocaml_bytesrw_sha1_init"
  external final : t -> hash = "ocaml_bytesrw_sha1_final"
  external update : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_sha1_update"
end

module Sha_1 = struct
  module State = struct
    type t = Sha1_ctx.t
    let make () = Sha1_ctx.init ()
    let update state s =
      if Bytes.Slice.is_eod s then () else
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and last = Bytes.Slice.length s in
      Sha1_ctx.update state b first last
  end

  let id = "sha1"
  let length = 20
  type t = Sha1_ctx.hash
  let value = Sha1_ctx.final
  let bytes b = Sha1_ctx.hash b 0 (Bytes.length b)
  let string s = bytes (Bytes.unsafe_of_string s)
  let slice s =
    let b = Bytes.Slice.bytes s in
    let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
    Sha1_ctx.hash b first length

  let reader r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ()) r

  let reads ?(state = State.make ()) r =
    Bytes.Reader.tap (State.update state) r, state

  let writes ?(state = State.make ()) w =
    Bytes.Writer.tap (State.update state) w, state

  let equal = String.equal
  let compare = String.compare
  let of_binary_string s = of_binary_string ~length s
  let to_binary_string = Fun.id
  let of_hex s = of_hex ~length of_binary_string s
  let pp = pp_hex
  let to_hex = string_to_hex
end

(* SHA-2 *)

module Sha2_ctx = struct
  type t (* Custom value whose data holds a SHA2_CTX C struct *)
  type h256 = string (* 32 bytes *)
  type h384 = string (* 48 bytes *)
  type h512 = string (* 64 bytes *)

  external hash256 : bytes -> int -> int -> h256 = "ocaml_bytesrw_sha256_hash"
  external init256 : unit -> t = "ocaml_bytesrw_sha256_init"
  external final256 : t -> h256 = "ocaml_bytesrw_sha256_final"
  external update256 : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_sha256_update"

  external hash384 : bytes -> int -> int -> h384 = "ocaml_bytesrw_sha384_hash"
  external init384 : unit -> t = "ocaml_bytesrw_sha384_init"
  external final384 : t -> h384 = "ocaml_bytesrw_sha384_final"
  external update384 : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_sha384_update"

  external hash512 : bytes -> int -> int -> h512 = "ocaml_bytesrw_sha512_hash"
  external init512 : unit -> t = "ocaml_bytesrw_sha512_init"
  external final512 : t -> h512 = "ocaml_bytesrw_sha512_final"
  external update512 : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_sha512_update"
end

module Sha_256 = struct
  module State = struct
    type t = Sha2_ctx.t
    let make () = Sha2_ctx.init256 ()
    let update state s =
      if Bytes.Slice.is_eod s then () else
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and last = Bytes.Slice.length s in
      Sha2_ctx.update256 state b first last
  end

  let id = "sha-256"
  let length = 32
  type t = Sha2_ctx.h256

  let value = Sha2_ctx.final256
  let bytes b = Sha2_ctx.hash256 b 0 (Bytes.length b)
  let string s = bytes (Bytes.unsafe_of_string s)
  let slice s =
    let b = Bytes.Slice.bytes s in
    let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
    Sha2_ctx.hash256 b first length

  let reader r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ()) r

  let reads ?(state = State.make ()) r =
    Bytes.Reader.tap (State.update state) r, state

  let writes ?(state = State.make ()) w =
    Bytes.Writer.tap (State.update state) w, state

  let equal = String.equal
  let compare = String.compare
  let of_binary_string s = of_binary_string ~length s
  let to_binary_string = Fun.id
  let of_hex s = of_hex ~length of_binary_string s
  let pp = pp_hex
  let to_hex = string_to_hex
end

module Sha_384 = struct
  module State = struct
    type t = Sha2_ctx.t
    let make () = Sha2_ctx.init384 ()
    let update state s =
      if Bytes.Slice.is_eod s then () else
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and last = Bytes.Slice.length s in
      Sha2_ctx.update384 state b first last
  end

  let id = "sha-384"
  let length = 48
  type t = Sha2_ctx.h384

  let value = Sha2_ctx.final384
  let bytes b = Sha2_ctx.hash384 b 0 (Bytes.length b)
  let string s = bytes (Bytes.unsafe_of_string s)
  let slice s =
    let b = Bytes.Slice.bytes s in
    let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
    Sha2_ctx.hash384 b first length

  let reader r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ()) r

  let reads ?(state = State.make ()) r =
    Bytes.Reader.tap (State.update state) r, state

  let writes ?(state = State.make ()) w =
    Bytes.Writer.tap (State.update state) w, state

  let equal = String.equal
  let compare = String.compare
  let of_binary_string s = of_binary_string ~length s
  let to_binary_string = Fun.id
  let of_hex s = of_hex ~length of_binary_string s
  let pp = pp_hex
  let to_hex = string_to_hex
end

module Sha_512 = struct
  module State = struct
    type t = Sha2_ctx.t
    let make () = Sha2_ctx.init512 ()
    let update state s =
      if Bytes.Slice.is_eod s then () else
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and last = Bytes.Slice.length s in
      Sha2_ctx.update512 state b first last
  end

  let id = "sha-512"
  let length = 64
  type t = Sha2_ctx.h512

  let value = Sha2_ctx.final512
  let bytes b = Sha2_ctx.hash512 b 0 (Bytes.length b)
  let string s = bytes (Bytes.unsafe_of_string s)
  let slice s =
    let b = Bytes.Slice.bytes s in
    let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
    Sha2_ctx.hash512 b first length

  let reader r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ()) r

  let reads ?(state = State.make ()) r =
    Bytes.Reader.tap (State.update state) r, state

  let writes ?(state = State.make ()) w =
    Bytes.Writer.tap (State.update state) w, state

  let equal = String.equal
  let compare = String.compare
  let of_binary_string s = of_binary_string ~length s
  let to_binary_string = Fun.id
  let of_hex s = of_hex ~length of_binary_string s
  let pp = pp_hex
  let to_hex = string_to_hex
end
