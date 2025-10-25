(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [zstd] streams (via [conf-zstd])

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc8878.html}[zstd]} compressed
    streams with the {{:http://zstd.net/}[libzstd]} C library.

    {b Positions.} The positions of readers and writers created
    by filters of this module default to [0]. *)

open Bytesrw

(** {1:errors Errors} *)

type Bytes.Stream.error += Error of string (** *)
(** The type for [zstd] stream errors.

    Except for the {{!lib}library parameters}, all functions of this
    module and resulting reader and writers may raise
    {!Bytesrw.Bytes.Stream.Error} with this error. *)

(** {1:decompress Decompress} *)

(** Decompression parameters. *)
module Dctx_params : sig
  type t
  (** The type for decompression parameters. *)

  val make : ?init:t -> ?window_log_max:int -> unit -> t
  (** [make ()] are the given compression parameters. Those unspecfied
      take the value of [init] which defaults to {!default}. See
      corresponding accessors for the default values. *)

  val default : t
  (** [default] are the default paramaters. See accessors for the
      default values. *)

  val window_log_max : t -> int
  (** [window_log_max] is the maximum back-reference distance in power
      of two allowed for decoding. Errors if the stream requires
      more. [0] is the default maximum window. This can be used to
      limit the memory used for decoding. *)

  (**/**)
  val unsafe_param : int -> int -> t -> t
  (**/**)
end

(** Decompression dictionaries. *)
module Ddict : sig
  type t
  (** The type for dictionaries. *)

  val of_binary_string : string -> t
  (** [of_binary_string s] is a dictionary from the binary data [s]. *)
end

val decompress_reads :
  ?all_frames:bool -> ?dict:Ddict.t -> ?params:Dctx_params.t -> unit ->
  Bytes.Reader.filter
(** [decompress_reads () r] filters the reads of [r] by decompressing
    [zstd] frames.
    {ul
    {- [dict] is the decompression dictionary, if any.}
    {- [params] defaults to {!Dctx_params.default}}
    {- [slice_length] defaults to {!dstream_out_size}.}
    {- If you get to create [r] and it has no constraints on its own
       use {!dstream_in_size} for its slices.}}
    If [all_frames] is:
    {ul
    {- [true] (default), this decompressses all frames until [r] returns
       {!Bytesrw.Bytes.Slice.eod} and concatenates the result.}
    {- [false] this decompresses a single frame. Once the resulting reader
       returns {!Bytesrw.Bytes.Slice.eod}, [r] is positioned exactly after
       the end of frame and can be used again to perform other non-filtered
       reads (e.g. a new [zstd] frame or other unrelated data).}} *)

val decompress_writes :
  ?dict:Ddict.t -> ?params:Dctx_params.t -> unit -> Bytes.Writer.filter
(** [decompress_writes () w ~eod] filters the writes on [w] by decompressing
    sequences of [zstd] frames until {!Bytesrw.Bytes.Slice.eod} is written.
    If [eod] is [false] the last {!Bytesrw.Bytes.Slice.eod} is not written
    on [w] and at this point [w] can be used again to perform other non-filtered
    writes.
    {ul
    {- [dict] is the decompression dictionary, if any.}
    {- [params] defaults to {!Dctx_params.default}}
    {- [slice_length] defaults to {!dstream_in_size}}
    {- Compressed slice lengths abides to [w]'s desire but if you get to
       create it and it has no constraints on its own use
       {!dstream_out_size}.}} *)

(** {1:compress Compress}

    {b Warning.} The default {!Cctx_params.default} compression
    parameters are those of the C library and do not perform
    checksums. If you want to compress so that the [zstd] command
    line tool can uncompress you need to checksum. See the
    example in the {{!page-index.quick}quick start}. *)

(** Compression parameters. *)
module Cctx_params : sig

  type clevel = int
  (** The type for compression levels. See {!val-clevel}. *)

  type t
  (** The type for compression parameters. *)

  val make :
    ?init:t -> ?checksum:bool -> ?clevel:clevel -> ?window_log:int -> unit -> t
  (** [make ()] are the given compression parameters. Those unspecfied
      take the value of [init] which defaults to {!default}. See
      corresponding accessors for the default values. *)

  val default : t
  (** [default] are the default parameters. See accessors for the
      default values. *)

  val checksum : t -> bool
  (** [checksum p] is [true] if frames are checksumed. Defaults
      to [false] {b Warning.} This mirrors the library default but does
      not mirror the [zstd] tool default. *)

  val clevel : t -> clevel
  (** [clevel p] is the compression level. Must be in
      the  {!min_clevel} to {!max_clevel} range. Defaults
      to {!default_clevel}. [0] means default compression level. *)

  val window_log : t -> int
  (** [window_log] is the maximal allowed back-reference distance in power
      of [2]. [0] means default window log. *)

  (**/**)
  val unsafe_param : int -> int -> t -> t
  (**/**)
end

(** Compression dictionaries. *)
module Cdict : sig
  type t
  (** The type for dictionaries. *)

  val of_binary_string : string -> t
  (** [of_binary_string s] is a dictionary from the binary data [s]. *)
end

val compress_reads :
  ?dict:Cdict.t -> ?params:Cctx_params.t -> unit -> Bytes.Reader.filter
(** [compress_reads () r] filters the reads of [r] by compressing them
    to a single [zstd] frame.
    {ul
    {- [dict] is the compression dictionary, if any.}
    {- [params] defaults to {!Cctx_params.default}.}
    {- [slice_length] defaults to {!cstream_out_size}.}
    {- If you get to create [r] and it has no constraints on its own
       use {!cstream_in_size} for its slices.}} *)

val compress_writes :
  ?dict:Cdict.t -> ?params:Cctx_params.t -> unit -> Bytes.Writer.filter
(** [compress_writes () w ~eod] filters the writes on [w] by compressing them
    to a single [zstd] frame until {!Bytesrw.Bytes.Slice.eod} is written.
    If [eod] is [false] the last {!Bytesrw.Bytes.Slice.eod} is not written
    on [w] and at this point [w] can be used again to perform non-filtered
    writes.
    {ul
    {- [dict] is the compression dictionary, if any.}
    {- [params] defaults to {!Cctx_params.default}.}
    {- [slice_length] defaults to {!cstream_in_size}.}
    {- Decompressed slice length abides to [w]'s desire but if you get to
       create it and it has no constraints on its own use
       {!cstream_out_size}.}} *)

(** {1:lib Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [libzstd] C library. *)

val min_clevel : unit -> Cctx_params.clevel
(** [min_clevel ()] is the minimum negative compression level allowed. *)

val max_clevel : unit -> Cctx_params.clevel
(** [max_clevel ()] is the maximum compression level available. *)

val default_clevel : unit -> Cctx_params.clevel
(** [default_clevel ()] is the default compression level. *)

val cstream_in_size : unit -> int
(** [cstream_in_size ()] is the recommended length of input slices
    on compression. *)

val cstream_out_size : unit -> int
(** [cstream_out_size ()] is the recommended length of output slices
    on compression. *)

val dstream_in_size : unit -> int
(** [dstream_in_size ()] is the recommended length of input slices
    on decompression. *)

val dstream_out_size : unit -> int
(** [dstream_out_size ()] is the recommended length of output
    slices on decompression. *)
