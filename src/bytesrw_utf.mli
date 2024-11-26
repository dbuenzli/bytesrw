(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** UTF streams.

    A few tools to deal with UTF encoded streams. For now just
    encoding guessing, more may be added in the future.

    Sample code for decoding UTF-8 with position tracking using a
    bytes reader and encoding UTF-8 with a bytes writer can be found
    {{:https://github.com/dbuenzli/bytesrw/blob/main/test/utf8codec.ml}here}. *)

open Bytesrw

(** {1:encodings Encodings} *)

(** Encoding specification. *)
module Encoding : sig

  type t = [
  | `Utf_8 (** UTF-8 *)
  | `Utf_16be (** UTF-16BE *)
  | `Utf_16le (** UTF-16LE *) ]
  (** The type for UTF encodings. *)

  val to_iana_charset : [< t | `Utf_16 ] -> string
  (** [to_iana_charaset e] is [e] as its
      {{:https://www.iana.org/assignments/character-sets/character-sets.xhtml}
      IANA character set name}. *)

  val pp : Format.formatter -> [< t | `Utf_16 ] -> unit
  (** [pp] formats encodings with {!to_iana_charset}. *)
end

(** {1:encoding_guess Encoding guess} *)

val guess_reader_encoding : Bytes.Reader.t -> Encoding.t
(** [guess_reader_encoding r] guesses the encoding at the stream
    position of [r] by {{!Bytesrw.Bytes.Reader.sniff}sniff}ing three
    bytes and applying {{!encoding_guess_heuristic}this
    heuristic} which is subject to change in the future. *)

(*
(** {1:validate Validate} *)

val ensure_reads : encoding -> Bytes.Reader.filter
(** [ensure_reads encoding r] filters the reads of [r] to make
    sure the stream is a valid [encoding] byte stream. Invalid
    byte sequences *)
*)

(** {1:encoding_guess_heuristic Encoding guess heurisitic}

    The heuristic is compatible with
    {{:http://unicode.org/glossary/#byte_order_mark}BOM} based
    recognition and the
    {{:http://tools.ietf.org/html/rfc4627#section-3}old} JSON encoding
    recognition (UTF-8 is mandated nowadays) that relies on ASCII
    being present at the beginning of the stream.

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
