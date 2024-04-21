(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

exception Error of string

external version : unit -> string = "ocaml_bytesrw_ZSTD_versionString"
external cstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamInSize"
external cstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamOutSize"
external dstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamInSize"
external dstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamOutSize"
external min_clevel : unit -> int = "ocaml_bytesrw_ZSTD_minCLevel"
external max_clevel : unit -> int = "ocaml_bytesrw_ZSTD_maxCLevel"
external default_clevel : unit -> int = "ocaml_bytesrw_ZSTD_defaultCLevel"

let compress_in_slice_length = cstream_in_size
let compress_out_slice_length = cstream_out_size
let decompress_in_slice_length = dstream_in_size
let decompress_out_slice_length = dstream_out_size

(* Buffers used to communicate with C. A counter part to
   ZSTD_inBuffer and ZSTD_outBuffer on the OCaml side. *)

module Zbuf = struct
  type t =
    { mutable bytes : Bytes.t;
      mutable size : int;
      mutable pos : int; (* next read or write position *) }

  let make ?slice_length default_length =
    let size = match slice_length with
    | None -> default_length ()
    | Some size -> if size <= 0 then invalid_arg "FIXME" else size
  in
  { bytes = Bytes.create size; size; pos = 0 }

  let empty () = { bytes = Bytes.empty; size = 0; pos = 0 }
  let src_is_empty buf = buf.pos >= buf.size
  let src_set_slice buf s =
    buf.bytes <- Bytes.Slice.bytes s;
    buf.size <- Bytes.Slice.first s + Bytes.Slice.length s;
    buf.pos <- Bytes.Slice.first s

  let dst_reset buf = buf.pos <- 0
  let dst_is_empty buf = buf.pos = 0
  let dst_was_full buf = buf.pos = buf.size
  let dst_to_slice buf =
    Bytes.Slice.make buf.bytes ~first:0 ~length:buf.pos
end


(* N.B. stubs raise Failure in case of error. *)

type dctx (* Custom value holding a ZSTD_DCtx, has a finalizer *)

external create_dctx : unit -> dctx = "ocaml_bytesrw_ZSTD_createDCtx"
external decompress : dctx -> src:Zbuf.t -> dst:Zbuf.t -> bool =
  "ocaml_bytesrw_ZSTD_decompressStream"

let decompress ?slice_length compressed = match create_dctx () with
| exception Failure e -> raise (Error e)
| ctx ->
    let src = Zbuf.empty () in
    let dst = Zbuf.make ?slice_length decompress_out_slice_length in
    let end_of_frame = ref false in
    let rec read () =
      let doit ctx ~src ~dst = match decompress ctx ~src ~dst with
      | exception Failure e -> raise (Error e)
      | eof ->
          end_of_frame := eof;
          if Zbuf.dst_is_empty dst
          then read ()
          else Zbuf.dst_to_slice dst
      in
      if Zbuf.dst_was_full dst || not (Zbuf.src_is_empty src)
      then (Zbuf.dst_reset dst; doit ctx ~src ~dst) else
      match Bytes.Reader.read compressed with
      | slice when Bytes.Slice.is_eod slice ->
          if !end_of_frame then slice else
          raise (Error "Unexpected end of compressed data")
      | slice ->
          Zbuf.dst_reset dst;
          Zbuf.src_set_slice src slice;
          doit ctx ~src ~dst
    in
    Bytes.Reader.make read
