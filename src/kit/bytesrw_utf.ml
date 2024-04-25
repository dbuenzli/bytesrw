(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* XXX add these things to Stdlib.Uchar *)

let uchar_max_utf_8_byte_length = 4
let uchar_utf_8_byte_decode_length byte = (* or utf_8_byte_length_of_byte *)
  if byte < 0x80 then 1 else if byte < 0xC2 then 0 else
  if byte < 0xE0 then 2 else if byte < 0xF0 then 3 else
  if byte < 0xF5 then 4 else 0

(* Encodings *)

type encoding =  [ `Utf_8 | `Utf_16be | `Utf_16le ]

let encoding_to_string = function
| `Utf_8 -> "UTF-8" | `Utf_16 -> "UTF-16" | `Utf_16be -> "UTF-16BE"
| `Utf_16le -> "UTF-16LE"

let pp_encoding ppf e = Format.pp_print_string ppf (encoding_to_string e)

(* Encoding guess *)

let guess_reader_encoding r = match Bytes.Reader.sniff 3 r with
| s when String.length s <= 1 -> `Utf_8 (* No or little input *)
| "\xEF\xBB\xBF" -> `Utf_8 (* BOM *)
| s when s.[0] = '\xFE' && s.[1] = '\xFF' -> `Utf_16be (* BOM *)
| s when s.[0] = '\xFF' && s.[1] = '\xFE' -> `Utf_16le (* BOM *)
| s when s.[0] = '\x00' && Char.code s.[1] > 0 -> `Utf_16be (* ASCII char *)
| s when Char.code s.[0] > 0 && s.[1] = '\x00' -> `Utf_16le (* ASCII char *)
| s when uchar_utf_8_byte_decode_length (Char.code s.[0]) <> 0 -> `Utf_8
| s -> `Utf_16be (* UTF-16 -> UTF-16BE *)
