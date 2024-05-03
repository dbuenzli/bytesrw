(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [deflate], [zlib] and [gzip] streams.

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc1951}[deflate]},
    {{:https://www.rfc-editor.org/rfc/rfc1950}[zlib]} and
    {{:https://www.rfc-editor.org/rfc/rfc1952}[gzip]}
    streams with the {{:https://zlib.net/}[zlib]} C library.

    {b Note.} The default [slice_length] of the readers
    created by this module is {!default_slice_length}. *)

open Bytesrw

(** {1:errors Errors} *)

type Bytes.Stream.error += Error of string (** *)
(** The type for [deflate], [zlib] and [gzip] stream errors.

    Except the {{!library}library parameters}, all functions of this
    module and resulting reader and writers may raise
    {!Bytesrw.Bytes.Stream.Error} with this error. *)

(** {1:fmt Formats} *)

type level = int
(** The type for compression levels.

    An integer between [-1] and [9], see these
    {{!compression_level}constants}. *)

(** {{:https://www.rfc-editor.org/rfc/rfc1951}[deflate]} streams. *)
module Deflate : sig

  (** {1:decompress Decompress} *)

  val decompress_reads :
    ?leftover:bool -> ?slice_length:Bytes.Slice.length -> Bytes.Reader.t ->
    Bytes.Reader.t
  (** [decompress_reads r] filters the reads of [r] by decompressing
      a [deflate] stream. If [leftover] is:
      {ul
      {- [false] (default), the reader errors if there is leftover data after
         the end of the [deflate] stream.}
      {- [true] the reader decompresses one [deflate] stream. Once the
         reader returns {!Bytes.Slice.eod}, [r] is positioned exactly
         after the end of the [deflate] stream and can be read again to
         perform other non-filtered reads.}} *)

  val decompress_writes :
    ?slice_length:Bytes.Slice.length -> Bytes.Writer.t -> Bytes.Writer.t
  (** [decompress_writes w] filters writes on [w] by decompressing a
      [deflate] stream until {!Bytes.Slice.eod} is written, if leftover
      data remains an error is raised. The last {!Bytes.Slice.eod} is not
      written on [w] and at this point [w] can be used again to perform other
      non-filtered writes. *)

  (** {1:compress Compress} *)

  val compress_reads :
    ?level:level -> ?slice_length:Bytes.Slice.length ->
    Bytes.Reader.t -> Bytes.Reader.t
  (** [compress_reads ~level r] filters the reads of [r] by compressing
      them to a [deflate] stream at level [level] (defaults to
      {!default_compression}). *)

  val compress_writes :
    ?level:level -> ?slice_length:Bytes.Slice.length ->
    Bytes.Writer.t -> Bytes.Writer.t
  (** [compress_writes ~level w] filters writes on [w] by compressing
      them to a [deflate] stream at level [level] (defaults to
      {!default_compression}) until {!Bytes.Slice.eod} is written. The
      latter is not written on [w] and at that point [w] can be used again to
      perform non-compressed writes. *)
end

(** {{:https://www.rfc-editor.org/rfc/rfc1950}[zlib]} streams. *)
module Zlib : sig

  (** {1:decompress Decompress} *)

  val decompress_reads :
    ?leftover:bool -> ?slice_length:Bytes.Slice.length -> Bytes.Reader.t ->
    Bytes.Reader.t
  (** [decompress_reads r] filters the reads of [r] by decompressing
      a [zlib] stream. If [leftover] is:
      {ul
      {- [false] (default), the reader errors if there is leftover data after
         the end of the [zlib] stream.}
      {- [true] the reader decompresses one [zlib] stream. Once the
         reader returns {!Bytes.Slice.eod}, [r] is positioned exactly
         after the end of the [zlib] stream and can be read again to
         perform other non-filtered reads.}} *)

  val decompress_writes :
    ?slice_length:Bytes.Slice.length -> Bytes.Writer.t -> Bytes.Writer.t
  (** [decompress_writes w] filters writes on [w] by decompressing a
      [zlib] stream until {!Bytes.Slice.eod} is written, if leftover
      data remains an error is raised. The last {!Bytes.Slice.eod} is not
      written on [w] and at this point [w] can be used again to perform other
      non-filtered writes. *)

  (** {1:compress Compress} *)

  val compress_reads :
    ?level:level -> ?slice_length:Bytes.Slice.length ->
    Bytes.Reader.t -> Bytes.Reader.t
  (** [compress_reads ~level r] filters the reads of [r] by compressing
      them to a [zlib] stream at level [level] (defaults to
      {!default_compression}). *)

  val compress_writes :
    ?level:level -> ?slice_length:Bytes.Slice.length ->
    Bytes.Writer.t -> Bytes.Writer.t
  (** [compress_writes ~level w] filters writes on [w] by compressing
      them to a [zlib] stream at level [level] (defaults to
      {!default_compression}) until {!Bytes.Slice.eod} is written. The
      latter is not written on [w] and at that point [w] can be used again to
      perform non-compressed writes. *)
end

(** {{:https://www.rfc-editor.org/rfc/rfc1952}[gzip]} streams.

    {b Note.} In general a [gzip] stream can be made of multiple,
    independently compressed, members. The way the module handles [gzip]
    member headers is described {{!Gzip.member_headers}here}. *)
module Gzip : sig

  (** {1:decompress Decompress} *)

  val decompress_reads :
    ?all_members:bool -> ?slice_length:Bytes.Slice.length -> Bytes.Reader.t ->
    Bytes.Reader.t
  (** [decompress_reads r] filters the reads of [r] by decompressing [gzip]
      members. If [all_members] is
      {ul
      {- [true] (default), this concatenates decompressed sequences of [gzip]
         members like [gunzip] would do and errors if there is leftover data.}
      {- [false] this decompresses a single [gzip] member. Once the resulting
         reader returns {!Bytes.Slice.eod}, [r] is positioned exactly
         after the end of the gzip member and can be used again to perform
         other non-filtered reads (e.g. a new [gzip] member or other unrelated
         data).}} *)

  val decompress_writes :
    ?slice_length:Bytes.Slice.length -> Bytes.Writer.t -> Bytes.Writer.t
  (** [decompress_writes w] filters the writes on [w] by decompressing
      sequences of [gzip] members until [Bytes.Slice.eod] is written.
      The latter is not written on [w] and at this point [w] can
      be used again to perform other non-filtered writes. *)

  (** {1:compress Compress} *)

  val compress_reads :
    ?level:level -> ?slice_length:Bytes.Slice.length ->
    Bytes.Reader.t -> Bytes.Reader.t
  (** [compress_reads ~level r] filters the reads of [r] by
      compressing them to as a single [gzip] member at level [level]
      (defaults to {!default_compression}). *)

  val compress_writes :
    ?level:level -> ?slice_length:Bytes.Slice.length ->
    Bytes.Writer.t -> Bytes.Writer.t
  (** [compress_writes ~level w] filters the writes on [w] by
      compressing them to a single [gzip] member at level [level]
      (defaults to {!default_compression}) until {!Bytes.Slice.eod} is
      written. The latter is not written on [w] and at this point [w]
      can be used again to perform other non-filtered writes.  *)

(** {1:member_headers Member headers}

    Currently no facility is provided to access [gzip] member
    headers. It seems those are little used in practice. However
    support is provided to read and write [gzip] streams
    member-by-member which is used by certain formats.

    On compression the member's header generated in the stream is
    [zlib]'s default header; see documentation of [deflateSetHeader]
    in the {{:https://www.zlib.net/manual.html} the manual}. Note that
    this watermarks the operating system in the stream (at least in
    v1.3.1 as of writing). *)
end

(** {1:library Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [zlib] C library. *)

val default_slice_length : int
(** [default_slice_length] is [128KB]. Note, this choice is made by
    [Bytesrw_zlib] not the [zlib] library. *)

(** {2:compression_level Compression levels} *)

val default_compression : level
(** [default_compression] is [-1], the default compression level.  The
    resulting level depends on the [zlib] library. *)

val no_compression : level
(** [no_compression] is [0], indicates no compression. *)

val best_speed : level
(** [best_speed] is [1], indicates fastest compression. *)

val best_compression : level
(** [best_compression] is [9], indicates best compression. *)
