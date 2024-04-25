(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** UTF streams.

    A few tools to deal with UTF encoded streams. *)

open Bytesrw

(** {1:encoding Encoding} *)

type encoding =  [ `Utf_8 | `Utf_16be | `Utf_16le ]
(** The type for UTF encodings. *)

val pp_encoding : Format.formatter -> [< encoding | `Utf_16 ] -> unit
(** [pp_encoding] formats encoding to its
    {{:https://www.iana.org/assignments/character-sets/character-sets.xhtml}IANA
    character setname}. *)

(** {1:encoding_guess Encoding guess} *)

val guess_reader_encoding : Bytes.Reader.t -> encoding
(** [guess_reader_encoding r] guesses the encoding at the stream
    position of [r] by {{!Bytesrw.Bytes.Reader.sniff}sniff}ing three
    bytes and applying {{!encoding_guess_heuristic}this
    heuristic} which is subject to change in the future. *)

(** {1:encoding_guess_heuristic Encoding guess heurisitic}

    {e Note, this was taken from Uutf. Twelve years laters I'm not sure it's
    the best way to go about it, in particular this was constrained by the
    making the JSON guess according to the old spec, JSON starts with ASCII
    but international text does not.}

    The heuristic is compatible with
    {{:http://unicode.org/glossary/#byte_order_mark}BOM} based
    recognition and the
    {{:http://tools.ietf.org/html/rfc4627#section-3}old} JSON encoding
    recognition that relies on ASCII being present at the beginning of
    the stream (JSON mandates UTF-8 nowadays).

    The heuristic looks at the first three bytes of input (or less if
    impossible) and takes the {e first} matching byte pattern in the
    table below.
{v
xx = any byte
.. = any byte or no byte (input too small)
pp = positive byte
uu = valid UTF-8 first byte

Bytes    | Guess     | Rationale
---------+-----------+-----------------------------------------------
EF BB BF | `UTF_8    | UTF-8 BOM
FE FF .. | `UTF_16BE | UTF-16BE BOM
FF FE .. | `UTF_16LE | UTF-16LE BOM
00 pp .. | `UTF_16BE | ASCII UTF-16BE and U+0000 is often forbidden
pp 00 .. | `UTF_16LE | ASCII UTF-16LE and U+0000 is often forbidden
uu .. .. | `UTF_8    | ASCII UTF-8 or valid UTF-8 first byte.
xx xx .. | `UTF_16BE | Not UTF-8 => UTF-16, no BOM => UTF-16BE
.. .. .. | `UTF_8    | Single malformed UTF-8 byte or no input.
v}
*)
