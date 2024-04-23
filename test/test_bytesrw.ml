(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Printf.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let test_read_length () =
  log "Testing Bytes.Reader.read_length";
  let r = Bytes.Reader.of_string ~slice_length:2  "1234" in
  assert (Bytes.Reader.read_length r = 0);
  ignore (Bytes.Reader.read r);
  assert (Bytes.Reader.read_length r = 2);
  ignore (Bytes.Reader.read r);
  assert (Bytes.Reader.read_length r = 4);
  ignore (Bytes.Reader.read r); ignore (Bytes.Reader.read r);
  assert (Bytes.Reader.read_length r = 4);
  ()

let test_written_length () =
  log "Testing Bytes.Writer.written_length";
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer ~slice_length:2 b in
  assert (Bytes.Writer.written_length w = 0);
  Bytes.Writer.write_string w "1234";
  assert (Bytes.Writer.written_length w = 4);
  Bytes.Writer.write_string w "56";
  assert (Bytes.Writer.written_length w = 6);
  Bytes.Writer.write_string w "";
  assert (Bytes.Writer.written_length w = 6);
  Bytes.Writer.write_eod w;
  assert (Bytes.Writer.written_length w = 6);
  assert (Buffer.contents b = "123456");
  ()

let main () =
  log "Testing Bytesrw";
  test_read_length ();
  test_written_length ();
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
