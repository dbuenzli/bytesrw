(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [deflate], [zlib] and [gzip] compressed streams.

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc1951}[deflate]},
    {{:https://www.rfc-editor.org/rfc/rfc1950}[zlib]} and
    {{:https://www.rfc-editor.org/rfc/rfc1952}[gzip]} compressed
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

(** {1:params Compression parameters} *)

type clevel = int
(** The type for compression levels. An integer between [-1] and [9]. *)

val default_compression : clevel
(** [default_compression] is [-1], the default compression level.  The
    resulting level depends on the zlib C library but currently
    defaults to [6]. *)

val no_compression : clevel
(** [clevel_no_compression] is [0], indicates no compression. *)

val best_speed : clevel
(** [best_speed] is [1], indicates fastest compression. *)

val best_compression : clevel
(** [best_compression] is [9], indicates best compression. *)

(** {1:fmt Formats} *)

(** [deflate] compressed streams. *)
module Deflate : sig

  (** {1:decompress Decompress} *)

  val decompress_reads :
    ?leftover:bool -> ?slice_length:Bytes.Slice.length -> Bytes.Reader.t ->
    Bytes.Reader.t
  (** [decompress_reads r] is a reader that decompresses the [deflate]
      compressed reads of [r].
      If [leftover] is:
      {ul
      {- [false] (default), the reader errors if there is leftover data after
         the end of the [deflate] stream.}
      {- [true] the reader decompresses one [deflate] stream. Once the
         reader returns {!Bytes.Slice.eod}, [r] is positioned exactly
         after the end of the [deflate] stream and can be read again to access
         leftover data.}} *)

  val decompress_writes :
    ?slice_length:Bytes.Slice.length -> Bytes.Writer.t -> Bytes.Writer.t
  (** [decompress_writes w] decompresses [deflate] compressed writes and
      writes the result on [w] until [Bytes.Slice.eod] is written. The
      latter is not written on [w] and there should be no leftover data
      at this point or an error is raised. *)

  (** {1:compress Compress} *)

  val compress_reads :
    ?slice_length:Bytes.Slice.length -> Bytes.Reader.t -> Bytes.Reader.t



end

(** [zlib] compressed stream. *)
module Zlib : sig

  (** {1:decompress Decompress} *)

  val decompress_reads :
    ?leftover:bool -> ?slice_length:Bytes.Slice.length -> Bytes.Reader.t ->
    Bytes.Reader.t
  (** [decompress_reads r] is a reader that decompresses the [zlib]
      compressed reads of [r].
      If [leftover] is:
      {ul
      {- [false] (default), the reader errors if there is leftover data after
         the end of the [zlib] stream.}
      {- [true] the reader decompresses one [zlib] stream. Once the
         reader returns {!Bytes.Slice.eod}, [r] is positioned exactly
         after the end of the [zlib] stream and can be read again to access
         leftover data.}} *)

  val decompress_writes :
    ?slice_length:Bytes.Slice.length -> Bytes.Writer.t -> Bytes.Writer.t
  (** [decompress_writes w] decompresses [zlib] compressed writes and
      writes the result on [w] until [Bytes.Slice.eod] is written. The
      latter is not written on [w] and there should be no leftover data
      at this point or an error is raised. *)

  (** {1:compress Compress} *)
end

(** {{:https://www.rfc-editor.org/rfc/rfc1952}[gzip]} compressed streams.

    {b Note.} Currently no facility is provided to access gzip member
    headers. It seems those are little used in practice. However support
    is provided to access compressed data member by member. *)
module Gzip : sig

  (** {1:decompress Decompress} *)

  val decompress_reads :
    ?all_members:bool -> ?slice_length:Bytes.Slice.length -> Bytes.Reader.t ->
    Bytes.Reader.t
  (** [decompress_reads r] decompresses the [gzip] compressed reads
      of [r]. If [all_members] is
      {ul
      {- [true] (default), this concatenates decompressed sequences of [gzip]
         members like [gunzip] would do and errors if there is leftover data.}
      {- [false] this decompresses a single [gzip] member. Once the resulting
         reader returns {!Bytes.Slice.eod}, [r] is positioned exactly
         after the end of the gzip member and can be read again to access
         leftover data.}} *)

  val decompress_writes :
    ?slice_length:Bytes.Slice.length -> Bytes.Writer.t -> Bytes.Writer.t
  (** [decompress_writes w] decompresses [gzip] compressed writes and
      writes the result on [w] until [Bytes.Slice.eod] is written. The
      latter is not written on [w]. *)
end

(** {1:library Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [zlib] C library. *)

val default_slice_length : int
(** [default_slice_length] is [128KB]. Note, this choice is made by
    [Bytesrw_zlib] not the [zlib] library. *)
