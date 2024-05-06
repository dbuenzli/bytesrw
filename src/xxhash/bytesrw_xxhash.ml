(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let strf = Printf.sprintf

external get_64u : String.t -> int -> int64 = "%caml_string_get64u"
external set_64u : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"
external swap_64 : int64 -> int64 = "%bswap_int64"
external noswap : int64 -> int64 = "%identity"
let layout = if Sys.big_endian then noswap else swap_64

let u64_to_binary_string t =
  let b = Bytes.create 8 in set_64u b 0 (layout t); Bytes.unsafe_to_string b

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
    (strf "Expected %d ASCII hexadecimal digit found %d characters" length slen)

let pp_hex ppf s =
  for i = 0 to String.length s - 1 do
    Format.fprintf ppf "%02x" (Char.code (s.[i]))
  done

let string_to_hex s = Format.asprintf "%a" pp_hex s

(* Library parameters *)

external version : unit -> int = "ocaml_bytesrw_XXH_versionNumber"
external xxh3_secret_size_min : unit -> int =
  "ocaml_bytesrw_XXH3_SECRET_SIZE_MIN"

let version () =
  let v = version () in
  let maj = v / (100 * 100) and rem = v mod (100 * 100) in
  let min = rem / 100 in
  Printf.sprintf "%d.%d.%d" maj min (rem mod 100)

module Xxh3_state = struct
  type t  (* Custom value holding a pointer to a finalized XXH3_state_t *)
  type seed = int64
  type secret = string
  type h64_hash = int64
  type h128_hash = string
  let no_seed = 0L

  external create : unit -> t = "ocaml_bytesrw_XXH3_createState"
  external copy : dst:t -> src:t -> unit = "ocaml_bytesrw_XXH3_copyState"
  external h64bits_reset : t -> unit = "ocaml_bytesrw_XXH3_64bits_reset"
  external h64bits_reset_with_seed : t -> seed -> unit =
    "ocaml_bytesrw_XXH3_64bits_reset_withSeed"

  external h64bits_reset_with_secret : t -> secret -> unit =
    "ocaml_bytesrw_XXH3_64bits_reset_withSecret"

  external h64bits_reset_with_secret_and_seed :
    t -> secret -> seed -> unit =
    "ocaml_bytesrw_XXH3_64bits_reset_withSecretandSeed"

  external h64bits_update : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_XXH3_64bits_update"

  external h64bits_digest : t -> h64_hash =
    "ocaml_bytesrw_XXH3_64bits_digest"

  external h128bits_reset : t -> unit = "ocaml_bytesrw_XXH3_128bits_reset"
  external h128bits_reset_with_seed : t -> seed -> unit =
    "ocaml_bytesrw_XXH3_128bits_reset_withSeed"

  external h128bits_reset_with_secret : t -> secret -> unit =
    "ocaml_bytesrw_XXH3_128bits_reset_withSecret"

  external h128bits_reset_with_secret_and_seed :
    t -> secret -> seed -> unit =
    "ocaml_bytesrw_XXH3_128bits_reset_withSecretandSeed"

  external h128bits_update : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_XXH3_128bits_update"

  external h128bits_digest : t -> h128_hash =
    "ocaml_bytesrw_XXH3_128bits_digest"
end

module Xxh3_64 = struct

  (* Hashes *)

  let id = "xxh3-64"
  let length = 8
  type seed = Xxh3_state.seed
  type secret = Xxh3_state.secret
  type t = Xxh3_state.h64_hash

  external seeded_hash : bytes -> int -> int -> seed -> t =
    "ocaml_bytesrw_XXH3_64bits_withSeed"

  module State = struct
    type t = Xxh3_state.t

    let make ?secret ?seed () =
      let state = Xxh3_state.create () in
      begin match secret, seed with
      | None, None -> Xxh3_state.h64bits_reset state
      | None, Some seed -> Xxh3_state.h64bits_reset_with_seed state seed
      | Some secret, None -> Xxh3_state.h64bits_reset_with_secret state secret
      | Some seed, Some secret ->
          Xxh3_state.h64bits_reset_with_secret_and_seed state seed secret
      end;
      state

    let update state s =
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and last = Bytes.Slice.length s in
      Xxh3_state.h64bits_update state b first last

    let copy src =
      let dst = Xxh3_state.create () in
      Xxh3_state.copy ~dst ~src; dst
  end

  let value = Xxh3_state.h64bits_digest

  (* Hashing *)

  let bytes ?(seed = Xxh3_state.no_seed) b =
    seeded_hash b 0 (Bytes.length b) seed

  let string ?seed s = bytes ?seed (Bytes.unsafe_of_string s)

  let slice ?(seed = Xxh3_state.no_seed) s =
    let b = Bytes.Slice.bytes s in
    seeded_hash b (Bytes.Slice.first s) (Bytes.Slice.length s) seed

  let reader ?seed r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ?seed ()) r

  (* Hashing streams *)

  let reads ?(state = State.make ()) r =
    let tap = State.update state in
    Bytes.Reader.tap tap r, state

  let writes ?(state = State.make ()) w =
    let tap = State.update state in
    Bytes.Writer.tap tap w, state

  (* Predicates and comparisons *)

  let equal = Int64.equal
  let compare = Int64.compare

  (* Converting *)

  let of_binary_string s =
    let slen = String.length s in
    if slen = length
    then Ok (layout (get_64u s 0))
    else Error (strf "Expected %d bytes, found %d" length slen)

  let to_binary_string = u64_to_binary_string
  let of_hex s = of_hex ~length of_binary_string s
  let to_hex h = Printf.sprintf "%Lx" h
  let to_uint64 = Fun.id
  let of_uint64 = Fun.id
  let pp ppf h = Format.fprintf ppf "%Lx" h
end

module Xxh3_128 = struct

  (* Hashes *)

  let id = "xxh3-128"
  let length = 16
  type seed = Xxh3_state.seed
  type secret = Xxh3_state.secret
  type t = Xxh3_state.h128_hash

  external seeded_hash : bytes -> int -> int -> seed -> t =
    "ocaml_bytesrw_XXH3_128bits_withSeed"

  module State = struct
    type t = Xxh3_state.t

    let make ?secret ?seed () =
      let state = Xxh3_state.create () in
      begin match secret, seed with
      | None, None -> Xxh3_state.h128bits_reset state
      | None, Some seed -> Xxh3_state.h128bits_reset_with_seed state seed
      | Some secret, None -> Xxh3_state.h128bits_reset_with_secret state secret
      | Some seed, Some secret ->
          Xxh3_state.h128bits_reset_with_secret_and_seed state seed secret
      end;
      state

    let update state s =
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and last = Bytes.Slice.length s in
      Xxh3_state.h64bits_update state b first last

    let copy src =
      let dst = Xxh3_state.create () in
      Xxh3_state.copy ~dst ~src; dst
  end

  let value = Xxh3_state.h128bits_digest

  (* Hashing *)

  let bytes ?(seed = Xxh3_state.no_seed) b =
    seeded_hash b 0 (Bytes.length b) seed

  let string ?seed s = bytes ?seed (Bytes.unsafe_of_string s)

  let slice ?(seed = Xxh3_state.no_seed) s =
    let b = Bytes.Slice.bytes s in
    seeded_hash b (Bytes.Slice.first s) (Bytes.Slice.length s) seed

  let reader ?seed r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ?seed ()) r

  (* Hashing streams *)

  let reads ?(state = State.make ()) r =
    let tap = State.update state in
    Bytes.Reader.tap tap r, state

  let writes ?(state = State.make ()) w =
    let tap = State.update state in
    Bytes.Writer.tap tap w, state

  (* Predicates and comparisons *)

  let equal = String.equal
  let compare = String.compare

  (* Converting *)

  let of_binary_string s =
    let slen = String.length s in
    if slen = length
    then Ok s
    else Error (Printf.sprintf "Expected %d bytes, found %d" length slen)

  let to_binary_string = Fun.id
  let of_hex s = of_hex ~length of_binary_string s
  let pp = pp_hex
  let to_hex = string_to_hex
end
