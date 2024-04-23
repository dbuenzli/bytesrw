(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Format.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let comp_test =
  (* Note this compressed data has a checksum. *)
  "\x28\xb5\x2f\xfd\x04\x58\x45\x00\x00\x10\x61\x61\x01\x00\x0c\xc0\x02\x61\
   \x36\xf8\xbb", String.make 30 'a'

let test_decompress_reads () =
  log "Testing Bytesrw_zstd.decompress_reads";
  let c = Bytes.Reader.of_string ~slice_length:3 (fst comp_test) in
  let d = Bytesrw_zstd.decompress_reads c in
  assert (snd comp_test = Bytes.Reader.to_string d)

let test_decompress_writes () =
  log "Testing Bytesrw_zstd.decompress_writes";
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer ~slice_length:10 b in
  let d = Bytesrw_zstd.decompress_writes w in
  let c = Bytes.Reader.of_string ~slice_length:3 (fst comp_test) in
  let () = Bytes.Writer.write_reader ~eod:true d c in
  assert (snd comp_test = Buffer.contents b)

let test_compress_reads () =
  log "Testing Bytesrw_zstd.compress_reads";
  let data = snd comp_test in
  let d = Bytes.Reader.of_string ~slice_length:4 data in
  let c = Bytesrw_zstd.compress_reads ~slice_length:2 d in
  let trip = Bytesrw_zstd.decompress_reads ~slice_length:3 c in
  assert (data = Bytes.Reader.to_string trip)

let test_compress_writes () =
  log "Testing Bytesrw_zstd.compress_writes";
  let data = snd comp_test in
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer ~slice_length:10 b in
  let dw = Bytesrw_zstd.decompress_writes w in
  let c = Bytesrw_zstd.compress_writes dw in
  let rdata = Bytes.Reader.of_string ~slice_length:3 data in
  let () = Bytes.Writer.write_reader ~eod:true c rdata in
  assert (data = Buffer.contents b)

let test_dictionary_support () =
  log "Testing dictionary support";
  let dict = "aaaaaaaa" in
  let data = "aaaaaaaabbbbbbbb" ^ "aaaaaaaa" ^ "aaaaaaaa" ^ "aaaaaaaa"in
  let cdict = Bytesrw_zstd.Cdict.of_binary_string dict in
  let ddict = Bytesrw_zstd.Ddict.of_binary_string dict in
  let datar = Bytes.Reader.of_string data in
  let c = Bytesrw_zstd.compress_reads ~dict:cdict datar in
  let d = Bytesrw_zstd.decompress_reads ~dict:ddict c in
  let d = Bytes.Reader.to_string d in
  assert (d = data);
  ()

let main () =
  log "Testing Bytesrw_zstd with libsztd %s" (Bytesrw_zstd.version ());
  test_decompress_reads ();
  test_decompress_writes ();
  test_compress_reads ();
  test_compress_writes ();
  test_dictionary_support ();
  Gc.full_major ();
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
