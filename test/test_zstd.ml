(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let test =
  "\x28\xb5\x2f\xfd\x04\x58\x45\x00\x00\x10\x61\x61\x01\x00\x0c\xc0\x02\x61\
   \x36\xf8\xbb", String.make 30 'a'

let test_decompress_reads () =
  print_endline "Testing Bytesrw_zstd.decompress_reads";
  let c = Bytes.Reader.of_string ~slice_length:3 (fst test) in
  let d = Bytesrw_zstd.decompress_reads c in
  let data = Bytes.Reader.to_string d in
  assert (snd test = data)

let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let test_decompress_writes () =
  print_endline "Testing Bytesrw_zstd.decompress_writes";
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer ~slice_length:10 b in
  let w = Bytes.Writer.trace_writes (tracer ~pp:Bytes.Slice.pp_text ~id:"D") w in
  let d = Bytesrw_zstd.decompress_writes w in
  let d = Bytes.Writer.trace_writes (tracer ~id:"C") d in
  let c = Bytes.Reader.of_string ~slice_length:3 (fst test) in
  let () = Bytes.Writer.write_reader d c in
  let () = Bytes.Writer.write_eod d in
  let data = Buffer.contents b in
  assert (snd test = data)

let main () =
  print_endline
    ("Testing Bytesrw_zstd with libsztd " ^ Bytesrw_zstd.version ());
  test_decompress_reads ();
  test_decompress_writes ();
  print_endline "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
