(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* XXX add these things to Stdlib.Uchar *)

let uchar_max_utf_8_byte_length = 4
let[@inline] uchar_utf_8_byte_decode_length = function
| '\x00' .. '\x7F' -> 1
| '\x80' .. '\xC1' -> 0
| '\xC2' .. '\xDF' -> 2
| '\xE0' .. '\xEF' -> 3
| '\xF0' .. '\xF4' -> 4
| _ -> 0

(* Encodings *)

module Encoding = struct
  type t =  [ `Utf_8 | `Utf_16be | `Utf_16le ]

  let to_iana_charset = function
  | `Utf_8 -> "UTF-8" | `Utf_16 -> "UTF-16" | `Utf_16be -> "UTF-16BE"
  | `Utf_16le -> "UTF-16LE"

  let pp ppf e = Format.pp_print_string ppf (to_iana_charset e)
end

(* Encoding guess *)

let guess_reader_encoding r = match Bytes.Reader.sniff 3 r with
| s when String.length s <= 1 -> `Utf_8 (* No or little input *)
| "\xEF\xBB\xBF" -> `Utf_8 (* BOM *)
| s when s.[0] = '\xFE' && s.[1] = '\xFF' -> `Utf_16be (* BOM *)
| s when s.[0] = '\xFF' && s.[1] = '\xFE' -> `Utf_16le (* BOM *)
| s when s.[0] = '\x00' && Char.code s.[1] > 0 -> `Utf_16be (* ASCII char *)
| s when Char.code s.[0] > 0 && s.[1] = '\x00' -> `Utf_16le (* ASCII char *)
| s when uchar_utf_8_byte_decode_length s.[0] <> 0 -> `Utf_8
| s -> `Utf_16be (* UTF-16 -> UTF-16BE *)

(* Validate

let ensure_utf_8_reads ?pos ?slice_length r =
  let read () = failwith "Unimplemented" in
  Bytes.Reader.make ?pos ?slice_length read

let ensure_utf_16be_reads ?pos ?slice_length r = failwith "Unimplemented"
let ensure_utf_16le_reads ?pos ?slice_length r = failwith "Unimplemented"

let ensure_reads = function
| `Utf_8 -> ensure_utf_8_reads
| `Utf_16be -> ensure_utf_16be_reads
| `Utf_16le -> ensure_utf_16le_reads
*)
