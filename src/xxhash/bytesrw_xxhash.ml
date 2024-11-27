(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

external get_64u : String.t -> int -> int64 = "%caml_string_get64u"
external set_64u : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"
external swap_64 : int64 -> int64 = "%bswap_int64"
external noswap : int64 -> int64 = "%identity"
let layout = if Sys.big_endian then noswap else swap_64
let u64_to_binary_string t =
  let b = Bytes.create 8 in set_64u b 0 (layout t); Bytes.unsafe_to_string b

(* Library parameters *)

external version : unit -> int = "ocaml_bytesrw_XXH_versionNumber"
external xxh3_secret_size_min : unit -> int =
  "ocaml_bytesrw_XXH3_SECRET_SIZE_MIN"

let version () =
  let v = version () in
  let maj = v / (100 * 100) and rem = v mod (100 * 100) in
  let min = rem / 100 in
  Printf.sprintf "%d.%d.%d" maj min (rem mod 100)

(* XXH3 module type *)

module type Xxh3 = sig
  val id : string
  val length : int
  type seed = int64
  type secret = string
  type t

  module State : sig
    type t
    val make : ?secret:secret -> ?seed:seed -> unit -> t
    val update : t -> Bytes.Slice.t -> unit
    val copy : t -> t
  end

  val value : State.t -> t
  val string : ?seed:seed -> string -> t
  val bytes : ?seed:seed -> bytes -> t
  val slice : ?seed:seed -> Bytes.Slice.t -> t
  val reader : ?seed:seed -> Bytes.Reader.t -> t
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

  external h64bits_digest : t -> h64_hash = "ocaml_bytesrw_XXH3_64bits_digest"

  external seeded_64hash : bytes -> int -> int -> seed -> h64_hash =
    "ocaml_bytesrw_XXH3_64bits_withSeed"

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

  external seeded_128hash : bytes -> int -> int -> seed -> h128_hash =
    "ocaml_bytesrw_XXH3_128bits_withSeed"
end

module Xxh3_64 = struct
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

  let id = "xxh3-64"
  let length = 8
  type seed = Xxh3_state.seed
  type secret = Xxh3_state.secret
  type t = Xxh3_state.h64_hash

  let value = Xxh3_state.h64bits_digest
  let bytes ?(seed = Xxh3_state.no_seed) b =
    Xxh3_state.seeded_64hash b 0 (Bytes.length b) seed

  let string ?seed s = bytes ?seed (Bytes.unsafe_of_string s)

  let slice ?(seed = Xxh3_state.no_seed) s =
    let b = Bytes.Slice.bytes s in
    Xxh3_state.seeded_64hash b (Bytes.Slice.first s) (Bytes.Slice.length s) seed

  let reader ?seed r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ?seed ()) r

  let reads ?(state = State.make ()) r =
    Bytes.Reader.tap (State.update state) r, state

  let writes ?(state = State.make ()) w =
    Bytes.Writer.tap (State.update state) w, state

  let equal = Int64.equal
  let compare = Int64.compare
  let of_binary_string s =
    match Bytesrw_hex.check_binary_string_length ~length s with
    | Error _ as e -> e
    | Ok s -> Ok (layout (get_64u s 0))

  let to_binary_string = u64_to_binary_string
  let of_hex s = match Bytesrw_hex.to_binary_string ~length s with
  | Error _ as e -> e
  | Ok s -> Ok (layout (get_64u s 0))

  let to_hex h = Printf.sprintf "%Lx" h
  let to_uint64 = Fun.id
  let of_uint64 = Fun.id
  let pp ppf h = Format.fprintf ppf "%Lx" h
end

module Xxh3_128 = struct
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

  let id = "xxh3-128"
  let length = 16
  type seed = Xxh3_state.seed
  type secret = Xxh3_state.secret
  type t = Xxh3_state.h128_hash

  let value = Xxh3_state.h128bits_digest

  let bytes ?(seed = Xxh3_state.no_seed) b =
    Xxh3_state.seeded_128hash b 0 (Bytes.length b) seed

  let string ?seed s = bytes ?seed (Bytes.unsafe_of_string s)

  let slice ?(seed = Xxh3_state.no_seed) s =
    let b = Bytes.Slice.bytes s in
    Xxh3_state.seeded_128hash
      b (Bytes.Slice.first s) (Bytes.Slice.length s) seed

  let reader ?seed r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make ?seed ()) r

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
