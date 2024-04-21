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

    Except for the {{!lib}library parameters}, all function of this
    module and resulting reader and writers may raise this exeception. *)

(** {1:simple Simple streams} *)

val decompress :
  ?slice_length:int -> Bytes.Reader.t -> Bytes.Reader.t
(** [decompress br] is a decompressed stream for the [zstd] compressed
    bytes of [br]. [slice_length] is the maximal slice length used by
    the result. It defaults to {!decompress_out_slice_length}. *)

(** {1:lib Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [libzstd] C library. *)

val compress_in_slice_length : unit -> int
(** [compress_in_slice_length ()] is the recommended length
    of decompressed slices on compression. *)

val compress_out_slice_length : unit -> int
(** [compress_out_slice_length ()] is the recommended length
    of compressed slices on compression. *)

val decompress_in_slice_length : unit -> int
(** [decompress_in_slice_length ()] is the recommended length
    of compressed slices on decompression. *)

val decompress_out_slice_length : unit -> int
(** [decompress_out_slice_length ()] is the recommended length of
    decompressed slices on decompression. *)
