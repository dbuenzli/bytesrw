(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Binary strings *)

let strf = Printf.sprintf

let to_binary_string' h = (* raises Failure *)
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

let err_len ~exp ~fnd =
  strf "Expected %d ASCII hexadecimal digits but found %d characters" exp fnd

let to_binary_string ?length hex =
  try match length with
  | None -> Ok (to_binary_string' hex)
  | Some len ->
      let exp = len * 2 in
      let fnd = String.length hex in
      if exp <> fnd then failwith (err_len ~exp ~fnd) else
      Ok (to_binary_string' hex)
  with
  | Failure e -> Error e

let pp_binary_string ppf s =
  for i = 0 to String.length s - 1
  do Format.fprintf ppf "%02x" (Char.code (s.[i])) done

let of_binary_string s = Format.asprintf "%a" pp_binary_string s

let check_binary_string_length ~length s =
  let len = String.length s in
  if len = length then Ok s else
  Error (strf "Expected %d bytes but found %d" length len)
