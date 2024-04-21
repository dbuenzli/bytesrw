(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

exception Error of string

external version : unit -> string = "ocaml_bytesrw_ZSTD_versionString"
external min_clevel : unit -> int = "ocaml_bytesrw_ZSTD_minCLevel"
external max_clevel : unit -> int = "ocaml_bytesrw_ZSTD_maxCLevel"
external default_clevel : unit -> int = "ocaml_bytesrw_ZSTD_defaultCLevel"
external cstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamInSize"
external cstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamOutSize"
external dstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamInSize"
external dstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamOutSize"
let csrc_slice_length = cstream_in_size
let cdst_slice_length = cstream_out_size
let dsrc_slice_length = dstream_in_size
let ddst_slice_length = dstream_out_size

module Zbuf = struct

  (* [Zbuf.t] values used to communicate with C. It is a counter part to
     ZSTD_inBuffer and ZSTD_outBuffer on the OCaml side. *)

  type t =
    { mutable bytes : Bytes.t;
      mutable size : int;
      mutable pos : int; (* next read or write position *) }

  let make ?slice_length default_length =
    let size = match slice_length with
    | None -> default_length ()
    | Some size -> Bytes.Slice.check_slice_length size
    in
    { bytes = Bytes.create size; size; pos = 0 }

  let empty () = { bytes = Bytes.empty; size = 0; pos = 0 }
  let src_is_consumed buf = buf.pos >= buf.size
  let src_set_slice buf s =
    buf.bytes <- Bytes.Slice.bytes s;
    buf.size <- Bytes.Slice.first s + Bytes.Slice.length s;
    buf.pos <- Bytes.Slice.first s

  let dst_clear buf = buf.pos <- 0
  let dst_is_empty buf = buf.pos = 0
  let dst_was_full buf = buf.pos = buf.size
  let dst_to_slice buf = Bytes.Slice.make buf.bytes ~first:0 ~length:buf.pos
end

(* N.B. stubs raise Failure in case of error. *)

type dctx (* Custom value holding a ZSTD_DCtx, has a finalizer *)
external create_dctx : unit -> dctx = "ocaml_bytesrw_ZSTD_createDCtx"
external decompress : dctx -> src:Zbuf.t -> dst:Zbuf.t -> bool =
  "ocaml_bytesrw_ZSTD_decompressStream" (* returns [true] at end of frames *)

let err_unexpected_eod () =
  raise (Error "Unexpected end of compressed data")

let decompress_reads ?stream_offset ?slice_length r = match create_dctx () with
| exception Failure e -> raise (Error e)
| ctx ->
    let src = Zbuf.empty () in
    let dst = Zbuf.make ?slice_length ddst_slice_length in
    let end_of_frame = ref false in
    let rec dec ctx ~src ~dst = match decompress ctx ~src ~dst with
    | exception Failure e -> raise (Error e)
    | eof ->
        end_of_frame := eof;
        if Zbuf.dst_is_empty dst then read () else Zbuf.dst_to_slice dst
    and read () =
      if Zbuf.dst_was_full dst || not (Zbuf.src_is_consumed src)
      then (Zbuf.dst_clear dst; dec ctx ~src ~dst) else
      match Bytes.Reader.read r with
      | slice when Bytes.Slice.is_eod slice ->
          if !end_of_frame then slice else err_unexpected_eod ()
      | slice ->
          Zbuf.dst_clear dst;
          Zbuf.src_set_slice src slice;
          dec ctx ~src ~dst
    in
    let stream_offset = Bytes.Reader.get_stream_offset ~none:r stream_offset in
    Bytes.Reader.make ~stream_offset ~slice_length:dst.Zbuf.size read

let decompress_writes ?stream_offset ?slice_length w = match create_dctx () with
| exception Failure e -> raise (Error e)
| ctx ->
    let src = Zbuf.empty () in
    let dst =
      let slice_length = Bytes.Writer.slice_length w in
      Zbuf.make ?slice_length ddst_slice_length
    in
    let end_of_frame = ref false in
    let rec dec ctx ~src ~dst = match decompress ctx ~src ~dst with
    | exception Failure e -> raise (Error e)
    | eof ->
        end_of_frame := eof;
        if Zbuf.dst_is_empty dst then () else begin
          Bytes.Writer.write w (Zbuf.dst_to_slice dst);
          if Zbuf.dst_was_full dst || not (Zbuf.src_is_consumed src)
          then (Zbuf.dst_clear dst; dec ctx ~src ~dst)
        end
    and write = function
    | slice when Bytes.Slice.is_eod slice ->
        if !end_of_frame then Bytes.Writer.write w slice else
        err_unexpected_eod ()
    | slice ->
        Zbuf.dst_clear dst;
        Zbuf.src_set_slice src slice;
        dec ctx ~src ~dst
    in
    let stream_offset = Bytes.Writer.get_stream_offset ~none:w stream_offset in
    let slice_length = match slice_length with
    | None -> dsrc_slice_length () | Some slen -> slen
    in
    Bytes.Writer.make ~stream_offset ~slice_length write
