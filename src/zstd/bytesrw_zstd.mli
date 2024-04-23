(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [zstd] compressed streams.

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc8878.html}[zstd]} compressed
    streams with the {{:http://zstd.net/}[libzstd]} C library. *)

open Bytesrw

(** {1:errors Errors} *)

exception Error of string
(** The exception for [zstd] errors.

    Except for the {{!lib}library parameters}, all functions of this
    module and resulting reader and writers may raise this
    exception. *)

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
  ?slice_length:Bytes.Slice.length -> ?dict:Ddict.t ->
  ?params:Dctx_params.t -> Bytes.Reader.t -> Bytes.Reader.t
(** [decompress_reads r] decompresses the [zstd] compressed reads of [r].
    {ul
    {- [dict] is the decompression dictionary, if any.}
    {- [params] defaults to {!Dctx_params.default}}
    {- [slice_length] defaults to {!ddst_slice_length}.}
    {- If you get to create [r] use {!dsrc_slice_length} for its slices.}}  *)

val decompress_writes :
  ?slice_length:Bytes.Slice.length -> ?dict:Ddict.t ->
  ?params:Dctx_params.t -> Bytes.Writer.t -> Bytes.Writer.t
(** [decompress_writes w] decompresses [zstd] compressed writes and
    writes the result on [w].
    {ul
    {- [dict] is the decompression dictionary, if any.}
    {- [params] defaults to {!Dctx_params.default}}
    {- [slice_length] defaults to {!dsrc_slice_length}}
    {- Compressed slice lengths abides to [w]'s desire but if you get to
       create it use {!ddst_slice_length}.}} *)

(** {1:compress Compress} *)

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
      to [false] {b Note.} This mirrors the library default but does
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
  ?slice_length:int -> ?dict:Cdict.t -> ?params:Cctx_params.t ->
  Bytes.Reader.t -> Bytes.Reader.t
(** [compress_reads r] compresses the reads of [r] with [zstd].
    {ul
    {- [dict] is the compression dictionary, if any.}
    {- [params] defaults to {!Cctx_params.default}.}
    {- [slice_length] defaults to {!cdst_slice_length}.}
    {- [stream_offset] defaults to [r]'s stream offset.}
    {- If you get to create [r] use {!csrc_slice_length} for its slices.}} *)

val compress_writes :
  ?slice_length:int -> ?dict:Cdict.t -> ?params:Cctx_params.t ->
  Bytes.Writer.t -> Bytes.Writer.t
(** [compress_writes w] compresses to [zstd] writes and writes the
    result on [w].
    {ul
    {- [dict] is the compression dictionary, if any.}
    {- [params] defaults to {!Cctx_params.default}.}
    {- [slice_length] defaults to {!dsrc_slice_length}.}
    {- Decompressed slice length abides to [w]'s desire but if you get to
       create it use {!ddst_slice_length}.}} *)

(** {1:lib Library parameters} *)

val version : unit -> string
(** [version ()] is the version of the [libzstd] C library. *)

val min_clevel : unit -> Cctx_params.clevel
(** [min_clevel ()] is the minimum negative compression level allowed. *)

val max_clevel : unit -> Cctx_params.clevel
(** [max_clevel ()] is the maximum compression level available. *)

val default_clevel : unit -> Cctx_params.clevel
(** [default_clevel ()] is the default compression level. *)

val csrc_slice_length : unit -> int
(** [csrc_slice_length ()] is the recommended length of source slices
    on compression. *)

val cdst_slice_length : unit -> int
(** [cdst_slice_length ()] is the recommended length of destination
    slices on compression. *)

val dsrc_slice_length : unit -> int
(** [dsrc_slice_length ()] is the recommended length of source slices
    on decompression. *)

val ddst_slice_length : unit -> int
(** [ddst_slice_length ()] is the recommended length of destination
    slices on decompression. *)
