(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Bytesrw

let stdio_compress_reads () =
  try
    let stdin = Bytes.Reader.of_in_channel In_channel.stdin in
    let stdout = Bytes.Writer.of_out_channel Out_channel.stdout in
    let params = Bytesrw_zstd.Cctx_params.make ~checksum:true () in
    let zstdr = Bytesrw_zstd.compress_reads ~params stdin in
    Bytes.Writer.write_reader ~eod:true stdout zstdr;
    Ok ()
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e
  | Sys_error e -> Error e

let stdio_compress_writes () =
  try
    let stdin = Bytes.Reader.of_in_channel In_channel.stdin in
    let stdout = Bytes.Writer.of_out_channel Out_channel.stdout in
    let params = Bytesrw_zstd.Cctx_params.make ~checksum:true () in
    let zstdw = Bytesrw_zstd.compress_writes ~params stdout in
    Bytes.Writer.write_reader ~eod:true zstdw stdin;
    Ok ()
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e
  | Sys_error e -> Error e
