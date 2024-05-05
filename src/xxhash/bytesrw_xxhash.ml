(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* Library parameters *)

external version : unit -> int = "ocaml_bytesrw_XXH_versionNumber"

let version () =
  let v = version () in
  let maj = v / (100 * 100) and rem = v mod (100 * 100) in
  let min = rem / 100 in
  Printf.sprintf "%d.%d.%d" maj min (rem mod 100)

external set_64u : Bytes.t -> int -> int64 -> unit = "%caml_string_set64u"
external swap_64 : int64 -> int64 = "%bswap_int64"
external noswap : int64 -> int64 = "%identity"
let layout = if Sys.big_endian then noswap else swap_64

let u64_to_bytes t =
  let b = Bytes.create 8 in
  set_64u b 0 (layout t); Bytes.unsafe_to_string b

module Xxh3_state = struct
  type t  (* Custom value holding a pointer to a finalized XXH3_state_t *)

  external create : unit -> t = "ocaml_bytesrw_XXH3_createState"
  external copy : dst:t -> src:t -> unit = "ocaml_bytesrw_XXH3_copyState"
  external h64bits_reset : t -> unit = "ocaml_bytesrw_XXH3_64bits_reset"
  external h64bits_update : t -> bytes -> int -> int -> unit =
    "ocaml_bytesrw_XXH3_64bits_update"
  external h64bits_digest : t -> int64 = "ocaml_bytesrw_XXH3_64bits_digest"
end
module Xxh3_64 = struct
  let id = "xxh3-64"
  let length = 8
  type seed = int64
  type secret = string
  type t = int64

  external seeded_hash : bytes -> int -> int -> seed -> t =
    "ocaml_bytesrw_XXH3_64bits_withSeed"

  module State = struct
    type t = Xxh3_state.t

    let make ?seed ?secret () =
      let state = Xxh3_state.create () in
      Xxh3_state.h64bits_reset state; state

    let update state s =
      let b = Bytes.Slice.bytes s in
      Xxh3_state.h64bits_update
        state b (Bytes.Slice.first s) (Bytes.Slice.length s)

    let copy src =
      let dst = Xxh3_state.create () in
      Xxh3_state.copy ~dst ~src; dst
  end

  let value = Xxh3_state.h64bits_digest

  let no_seed = 0L

  let bytes ?(seed = no_seed) b =
    seeded_hash b 0 (Bytes.length b) seed

  let string ?(seed = no_seed) s =
    seeded_hash (Bytes.unsafe_of_string s) 0 (String.length s) seed

  let slice ?(seed = no_seed) s =
    let b = Bytes.Slice.bytes s in
    seeded_hash b (Bytes.Slice.first s) (Bytes.Slice.length s) seed

  let reads ?(state = State.make ()) r =
    let tap = State.update state in
    Bytes.Reader.trace_reads tap r, state

  let writes ?(state = State.make ()) w =
    let tap = State.update state in
    Bytes.Writer.trace_writes tap w, state

  let equal = Int64.equal
  let compare = Int64.compare
  let to_binary_string = u64_to_bytes
  let to_int64 = Fun.id
end
