(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* Errors *)

exception Panic of string

let panic_printer = function
| Panic e -> Some (Printf.sprintf "Bytesrw_sysrandom.Panic <%s>" e)
| _ -> None

let init () = Printexc.register_printer panic_printer
let () = init ()

(* Primitives *)

external getrandom : Bytes.t -> first:int -> length:int -> string option =
  "ocaml_bytesrw_sysrandom_getrandom"

external getentropy : Bytes.t -> first:int -> length:int -> string option =
  "ocaml_bytesrw_sysrandom_getentropy"

let set_random s =
  let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
  match getrandom (Bytes.Slice.bytes s) ~first ~length with
  | None -> () | Some err -> raise (Panic err)

let set_entropy s =
  let first = Bytes.Slice.first s and n = Bytes.Slice.length s in
  if n > 256 (* This is the minimal value mandated by POSIX *)
  then invalid_arg (Printf.sprintf "Requested %d bytes exceeds 256 limit" n)
  else
  match getentropy (Bytes.Slice.bytes s) ~first ~length:n with
  | None -> () | Some err -> raise (Panic err)

(* Pseudorandom bytes *)

let check_length l =
  if l >= 0 then l else
  invalid_arg (Printf.sprintf "Invalid random length: %d is negative" l)

let reads ?pos ?slice_length ?length () =
  let default = Bytes.Slice.default_length in
  let slice_length = Option.value ~default slice_length in
  match length with
  | None -> (* infinite stream *)
      let slice = Bytes.Slice.of_bytes (Bytes.create slice_length) in
      let read () = set_random slice; slice in
      Bytes.Reader.make ?pos ~slice_length read
  | Some 0 -> Bytes.Reader.empty ?pos ~slice_length ()
  | Some len ->
      let slice_length = Int.min (check_length len) slice_length in
      let slice = Bytes.Slice.of_bytes_or_eod (Bytes.create slice_length) in
      let rem = ref len in
      let read () = match !rem with
      | 0 -> Bytes.Slice.eod
      | n ->
          let slice =
            if n >= Bytes.Slice.length slice then slice else
            Bytes.Slice.make (Bytes.Slice.bytes slice) ~first:0 ~length:n
          in
          rem := n - Bytes.Slice.length slice;
          set_random slice; slice
      in
      Bytes.Reader.make ?pos ~slice_length read

let bytes n =
  let b = Bytes.create n in
  match getrandom b ~first:0 ~length:n with
  | None -> b | Some err -> raise (Panic err)

let string n = Bytes.unsafe_to_string (bytes n)
