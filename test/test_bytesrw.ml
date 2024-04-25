(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Printf.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let bos s = Bytes.of_string s

let reader_of_list ss =
  Bytes.Reader.of_slice_seq (List.to_seq (List.map Bytes.Slice.of_string ss))

let assert_invalid_arg f =
  try f (); log "Expression did not raise"; assert false with
  | Invalid_argument _ -> ()

let eq_slice sl s =
  let sl = Bytes.Slice.to_string sl in
  if sl <> s then (log "%s <> %s" sl s; assert false) else ()

let eq_str s0 s1 =
  if s0 <> s1 then (log "%S <> %S" s0 s1; assert false) else ()

let eq_eod sl =
  if not (Bytes.Slice.is_eod sl)
  then (log "%s <> Slice.eod" (Bytes.Slice.to_string sl); assert false)

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

let test_push_backs () =
  log "Testing Bytes.Reader.push_back";
  let r () = reader_of_list ["a"; "bb"; "ccc"] in
  let r0 = r () in
  eq_slice (Bytes.Reader.read r0) "a";
  let s = Bytes.Reader.read r0 in
  eq_slice s "bb";
  assert (Bytes.Reader.pos r0 = 3);
  Bytes.Reader.push_back r0 s;
  assert (Bytes.Reader.pos r0 = 1);
  eq_slice (Bytes.Reader.read r0) "bb";
  assert (Bytes.Reader.pos r0 = 3);
  eq_slice (Bytes.Reader.read r0) "ccc";
  eq_eod (Bytes.Reader.read r0);
  let r1 = r () in
  Bytes.Reader.push_back r1 (Bytes.Slice.of_string "zz");
  assert (Bytes.Reader.pos r1 = -2);
  eq_slice (Bytes.Reader.read r1) "zz";
  assert (Bytes.Reader.pos r1 = 0);
  eq_slice (Bytes.Reader.read r1) "a";
  assert (Bytes.Reader.pos r1 = 1);
  (assert_invalid_arg @@ fun () ->
   Bytes.Reader.push_back r1 Bytes.Slice.eod);
  ()

let test_sniff () =
  log "Testing Bytes.Reader.sniff";
  let r () = reader_of_list ["a"; "bb"; "ccc"] in
  let r0 = r () in
  eq_str (Bytes.Reader.sniff 2 r0) "ab";
  eq_str (Bytes.Reader.to_string r0) "abbccc";
  let r1 = r () in
  eq_str (Bytes.Reader.sniff 1 r1) "a";
  eq_str (Bytes.Reader.to_string r1) "abbccc";
  let r2 = r () in
  eq_str (Bytes.Reader.sniff 4 r2) "abbc";
  eq_str (Bytes.Reader.to_string r2) "abbccc";
  (assert_invalid_arg @@ fun () -> Bytes.Reader.sniff (-1) (r ()));
  let r4 = reader_of_list ["a"; "bb"] in
  eq_str (Bytes.Reader.sniff 4 r4) "abb";
  eq_str (Bytes.Reader.to_string r4) "abb";
  let r5 = Bytes.Reader.empty () in
  eq_str (Bytes.Reader.sniff 4 r5) "";
  eq_str (Bytes.Reader.to_string r5) "";
  ()

let main () =
  log "Testing Bytesrw";
  test_slices ();
  test_read_length ();
  test_written_length ();
  test_read_fun_eod ();
  test_write_fun_eod ();
  test_push_backs ();
  test_sniff ();
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
