(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [zstd] compressed byte streams.

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc8878.html}[zstd]} compressed
    byte streams with the {{:https://zstd.net/}[libzstd]} C library. *)

open Bytesrw

(** {1:errors Errors} *)

exception Error of string
(** The exception for [zstd] errors.

    Except for the {{!lib}library parameters}, all functions of this
    module and resulting reader and writers may raise this exeception.

    {b TODO.} Is there a case to distinguish unexpected end of data ? Add
    stream offset. *)

(** {1:decompress Decompress} *)

val decompress_reads :
  ?stream_offset:int -> ?slice_length:int -> Bytes.Reader.t -> Bytes.Reader.t
(** [decompress_reads r] decompresses the [zstd] compressed reads of [r].

    [slice_length] and [stream_offset] are used by the resulting
    reader. [slice_length] defaults to {!ddst_slice_length}; if you
    get to create [r] use {!dsrc_slice_length} for its
    slices. [stream_offset] defaults to the stream offset of [r]. *)

val decompress_writes :
  ?stream_offset:int -> ?slice_length:int -> Bytes.Writer.t -> Bytes.Writer.t
(** [decompress_writes w] decompresses the [zstd] compressed writes made
    on [w].

    [slice_length] and [stream_offset] are used by the resulting
    writer. [slice_length] defaults to {!dsrc_slice_length}.
    Decompressed slices will abide to [w]'s desire but if you get to
    create it use {!ddst_slice_length}. [stream_offset] defaults to
    the stream offset of [w]. *)

(** {1:lib Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [libzstd] C library. *)

val csrc_slice_length : unit -> int
(** [csrc_slice_length ()] is the recommended length of source slices on
    compression. *)

val cdst_slice_length : unit -> int
(** [cdst_slice_length ()] is the recommended length of destination
    slices on compression. *)

val dsrc_slice_length : unit -> int
(** [dsrc_slice_length ()] is the recommended length of source slices
    on decompression. *)

val ddst_slice_length : unit -> int
(** [ddst_slice_length ()] is the recommended length of destination
    slices on decompression. *)
