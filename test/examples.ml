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
    let zstdr = Bytesrw_zstd.compress_reads ~params () stdin in
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
    let zstdw = Bytesrw_zstd.compress_writes ~params () ~eod:true stdout in
    Bytes.Writer.write_reader ~eod:true zstdw stdin;
    Ok ()
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e
  | Sys_error e -> Error e

let id s =
  let filters = Bytesrw_zstd.[compress_reads (); decompress_reads ()] in
  Bytes.Reader.filter_string filters s

let id s =
  let filters = Bytesrw_zstd.[decompress_writes (); compress_writes ()] in
  Bytes.Writer.filter_string filters s

let blake3_and_compress ~plain =
  try
    let plain, blake3 = Bytesrw_blake3.Blake3.reads plain in
    let comp = Bytesrw_zstd.compress_reads () plain in
    let comp = Bytes.Reader.to_string comp in
    Ok (comp, Bytesrw_blake3.Blake3.value blake3)
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let decompress_and_blake3 ~comp =
  try
    let plain = Bytesrw_zstd.decompress_reads () comp in
    let r, blake3 = Bytesrw_blake3.Blake3.reads plain in
    let s = Bytes.Reader.to_string r in
    Ok (s, Bytesrw_blake3.Blake3.value blake3)
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let limited_decompress ~quota ~comp =
  let buf = Buffer.create quota in
  try
    let plain = Bytesrw_zstd.decompress_reads () comp in
    let () = Bytes.Reader.add_to_buffer buf (Bytes.Reader.limit quota plain) in
    Ok (`Data (Buffer.contents buf))
  with
  |  Bytes.Stream.Error (Bytes.Stream.Limit _quota, _) ->
      Ok (`Quota_exceeded (Buffer.contents buf))
  |  Bytes.Stream.Error e -> Bytes.Stream.error_to_result e
