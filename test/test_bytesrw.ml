(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Printf.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let assert_invalid_arg f =
  try f (); log "Expression did not raise"; assert false with
  | Invalid_argument _ -> ()

let eq_slice sl s =
  let sl = Bytes.Slice.to_string sl in
  if sl <> s then (log "%s <> %s" sl s; assert false) else ()

let eq_eod sl =
  if not (Bytes.Slice.is_eod sl)
  then (log "%s <> Slice.eod" (Bytes.Slice.to_string sl); assert false)

let bos s = Bytes.of_string s

let test_slices () =
  let err = assert_invalid_arg and eq = eq_slice in
  log "Testing Bytes.Slices";
  eq (Bytes.Slice.make (bos "1234") ~first:1 ~length:2) "23";
  (err @@ fun () -> Bytes.Slice.make (bos "1234") ~first:1 ~length:0);
  eq_eod (Bytes.Slice.make_or_eod (bos "1234") ~first:1 ~length:0);
  eq (Bytes.Slice.of_bytes ~first:1 (bos "1234") ) "234";
  (err @@ fun () -> Bytes.Slice.of_bytes ~first:4 (bos "1234"));
  eq_eod (Bytes.Slice.of_bytes_or_eod ~first:4 (bos "1234"));
  ()

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

let test_read_fun_eod () =
  log "Testing Bytes.Reader.t end of stream";
  let once = ref false in
  let read () =
    if !once then assert false else (once := true; Bytes.Slice.eod)
  in
  let r = Bytes.Reader.make read in
  assert (Bytes.Slice.is_eod (Bytes.Reader.read r));
  assert (Bytes.Slice.is_eod (Bytes.Reader.read r));
  assert (Bytes.Slice.is_eod (Bytes.Reader.read r));
  ()

let test_write_fun_eod () =
  log "Testing Bytes.Write.t end of stream";
  let once = ref false in
  let write _slice = if !once then assert false else once := true in
  let w = Bytes.Writer.make write in
  Bytes.Writer.write w Bytes.Slice.eod;
  Bytes.Writer.write w Bytes.Slice.eod;
  Bytes.Writer.write w Bytes.Slice.eod;
  (assert_invalid_arg @@ fun () ->
   Bytes.Writer.write w (Bytes.Slice.of_bytes (bos "nooooo!")));
  ()


let main () =
  log "Testing Bytesrw";
  test_slices ();
  test_read_length ();
  test_written_length ();
  test_read_fun_eod ();
  test_write_fun_eod ();
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
