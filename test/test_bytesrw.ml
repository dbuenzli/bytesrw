(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw
open B0_testing

let bos s = Bytes.of_string s

let reader_of_list ss =
  Bytes.Reader.of_slice_seq (List.to_seq (List.map Bytes.Slice.of_string ss))

let reader_to_list r =
  List.of_seq (Seq.map Bytes.Slice.to_string (Bytes.Reader.to_slice_seq r))

let test_stream_error f =
  let is_exn = function Bytes.Stream.Error _ -> true | _ -> false in
  Test.raises' is_exn f

let eq_slice ?__POS__ sl s =
  Test.string ?__POS__ (Bytes.Slice.to_string sl) s

let eq_slices ?__POS__ r sl =
  let sl' = reader_to_list r in
  if sl' <> sl then (List.iter2 (Test.string ?__POS__) sl' sl)

let eq_eod ?__POS__ sl =
  if not (Bytes.Slice.is_eod sl)
  then Test.fail ?__POS__ "%s <> Slice.eod" (Bytes.Slice.to_string sl)
  else Test.pass ?__POS__ ()

let test_slice_make () =
  Test.test "Bytes.Slices.make[_or_eod]" @@ fun () ->
  let err = Test.invalid_arg in
  let s1234 = bos "1234" in
  (err ~__POS__ @@ fun () -> Bytes.Slice.make (bos "") ~first:0 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make s1234 ~first:0 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make s1234 ~first:1 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make s1234 ~first:2 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make s1234 ~first:3 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make s1234 ~first:4 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make s1234 ~first:5 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make_or_eod s1234 ~first:5 ~length:0);
  eq_slice (Bytes.Slice.make s1234 ~first:0 ~length:2) "12";
  eq_slice (Bytes.Slice.make s1234 ~first:1 ~length:2) "23";
  eq_slice (Bytes.Slice.make s1234 ~first:2 ~length:2) "34";
  (* or_eod *)
  eq_eod (Bytes.Slice.make_or_eod (bos "") ~first:0 ~length:0);
  eq_eod (Bytes.Slice.make_or_eod s1234 ~first:0 ~length:0);
  eq_eod (Bytes.Slice.make_or_eod s1234 ~first:1 ~length:0);
  eq_eod (Bytes.Slice.make_or_eod s1234 ~first:2 ~length:0);
  eq_eod (Bytes.Slice.make_or_eod s1234 ~first:3 ~length:0);
  eq_eod (Bytes.Slice.make_or_eod s1234 ~first:4 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make_or_eod s1234 ~first:5 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.make s1234 ~first:3 ~length:2);
  eq_slice (Bytes.Slice.make_or_eod s1234 ~first:0 ~length:2) "12";
  eq_slice (Bytes.Slice.make_or_eod s1234 ~first:1 ~length:2) "23";
  eq_slice (Bytes.Slice.make_or_eod s1234 ~first:2 ~length:2) "34";
  (err ~__POS__ @@ fun () -> Bytes.Slice.make_or_eod s1234 ~first:3 ~length:2)

let test_slice_sub () =
  Test.test "Bytes.Slices.sub[_or_eod]" @@ fun () ->
  let err = Test.invalid_arg in
  let slice s ~first ~length = Bytes.Slice.make_or_eod (bos s) ~first ~length in
  let s234 = slice "12345" ~first:1 ~length:3 in
  (err ~__POS__ @@ fun () -> Bytes.Slice.sub s234 ~first:0 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.sub s234 ~first:1 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.sub s234 ~first:2 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.sub s234 ~first:3 ~length:0);
  (err ~__POS__ @@ fun () -> Bytes.Slice.sub s234 ~first:4 ~length:0);
  eq_slice (Bytes.Slice.sub s234 ~first:0 ~length:1) "2";
  eq_slice (Bytes.Slice.sub s234 ~first:1 ~length:1) "3";
  eq_slice (Bytes.Slice.sub s234 ~first:2 ~length:1) "4";
  (err ~__POS__ @@ fun () -> Bytes.Slice.sub s234 ~first:3 ~length:1);
  (err ~__POS__ @@
   fun () -> eq_slice (Bytes.Slice.sub s234 ~first:4 ~length:1));
  (* or_eod *)
  eq_eod (Bytes.Slice.sub_or_eod s234 ~first:0 ~length:0);
  eq_eod (Bytes.Slice.sub_or_eod s234 ~first:1 ~length:0);
  eq_eod (Bytes.Slice.sub_or_eod s234 ~first:2 ~length:0);
  eq_eod (Bytes.Slice.sub_or_eod s234 ~first:3 ~length:0);
  (err ~__POS__ @@
   fun () -> Bytes.Slice.sub_or_eod s234 ~first:4 ~length:0);
  eq_slice (Bytes.Slice.sub_or_eod s234 ~first:0 ~length:1) "2";
  eq_slice (Bytes.Slice.sub_or_eod s234 ~first:1 ~length:1) "3";
  eq_slice (Bytes.Slice.sub_or_eod s234 ~first:2 ~length:1) "4";
  (err ~__POS__ @@ fun () -> Bytes.Slice.sub_or_eod s234 ~first:3 ~length:1);
  (err ~__POS__ @@
   fun () -> eq_slice (Bytes.Slice.sub_or_eod s234 ~first:4 ~length:1));
  ()

let test_slice_of_bytes () =
  Test.test "Bytes.Slices.of_bytes[_or_eod]" @@ fun () ->
  let err = Test.invalid_arg in
  eq_slice (Bytes.Slice.of_bytes ~first:1 (bos "1234") ) "234";
  (err ~__POS__ @@ fun () -> Bytes.Slice.of_bytes ~first:4 (bos "1234"));
  eq_eod (Bytes.Slice.of_bytes_or_eod ~first:4 (bos "1234"));
  ()


let test_slice_compare () =
  Test.test "Bytes.Slice.compare" @@ fun () ->
  let test_cmp ?__POS__ s0 s1 v =
    Test.int ?__POS__ (Bytes.Slice.compare s0 s1) v;
    Test.int ?__POS__ (Bytes.Slice.compare s1 s0) (Int.neg v)
  in
  let snil = Bytes.Slice.make_or_eod (bos "1234") ~first:2 ~length:0 in
  let s1 = Bytes.Slice.make (bos "1234") ~first:0 ~length:1 in
  let s12 = Bytes.Slice.make (bos "1234") ~first:0 ~length:2 in
  let s23 = Bytes.Slice.make (bos "1234") ~first:1 ~length:2 in
  let s234 = Bytes.Slice.make (bos "1234") ~first:1 ~length:3 in
  let s1234 = Bytes.Slice.make (bos "1234") ~first:0 ~length:4 in
  (* snil *)
  test_cmp snil snil 0 ~__POS__; test_cmp snil s1 (-1) ~__POS__;
  test_cmp snil s12 (-1) ~__POS__; test_cmp snil s23 (-1) ~__POS__;
  test_cmp snil s234 (-1) ~__POS__; test_cmp snil s1234 (-1) ~__POS__;
  (* s1 *)
  test_cmp s1 s1 0 ~__POS__; test_cmp s1 s12 (-1) ~__POS__;
  test_cmp s1 s23 (-1) ~__POS__; test_cmp s1 s234 (-1) ~__POS__;
  test_cmp s1 s1234 (-1) ~__POS__;
  (* s12 *)
  test_cmp s12 s12 0 ~__POS__; test_cmp s12 s23 (-1) ~__POS__;
  test_cmp s12 s234 (-1) ~__POS__; test_cmp s12 s1234 (-1) ~__POS__;
  (* s23 *)
  test_cmp s23 s23 0 ~__POS__; test_cmp s12 s234 (-1) ~__POS__;
  test_cmp s12 s1234 (-1) ~__POS__;
  (* s234 *)
  test_cmp s234 s234 0 ~__POS__; test_cmp s234 s1234 (-1) ~__POS__;
  (* s1234 *)
  test_cmp s1234 s1234 0 ~__POS__;
  ()

let test_reader_reslice () =
  Test.test "Bytes.Reader.reslice" @@ fun () ->
  let test ?__POS__ ~slice_length sls rsl =
    let r = Bytes.Reader.reslice ~slice_length (reader_of_list sls) in
    eq_slices ?__POS__ r rsl
  in
  test ~slice_length:2 [] [] ~__POS__;
  test ~slice_length:2 ["abc"] ["ab"; "c"] ~__POS__;
  test ~slice_length:2 ["abcd"; "ef"] ["ab"; "cd"; "ef"] ~__POS__;
  test ~slice_length:1 ["abcd"; "ef"] ["a"; "b"; "c"; "d"; "e"; "f"] ~__POS__;
  test ~slice_length:3 ["abcd"; "efg"] ["abc"; "def"; "g"] ~__POS__;
  test ~slice_length:2 ["abcdef"] ["ab"; "cd"; "ef"] ~__POS__;
  test ~slice_length:3 ["a"; "b";"c"] ["abc"] ~__POS__;
  test ~slice_length:2 ["a"; "b"; "c"] ["ab"; "c"] ~__POS__;
  test ~slice_length:2 ["ab"; "bc"; "de"] ["ab"; "bc"; "de"] ~__POS__;
  ()

let test_reader_compare () =
  Test.test "Bytes.Reader.compare" @@ fun () ->
  let test ?__POS__ l0 l1 eq =
    let r0 = reader_of_list l0 and r1 = reader_of_list l1 in
    Test.int ?__POS__ (Bytes.Reader.compare r0 r1) eq;
    let r0 = reader_of_list l0 and r1 = reader_of_list l1 in
    Test.int ?__POS__ (Bytes.Reader.compare r1 r0) (Int.neg eq);
  in
  test ["abc"] ["a"; "bc"] 0 ~__POS__;
  test ["a"] ["a"; "bc"] (-1) ~__POS__;
  test ["abcd"] ["a"; "bc"] 1 ~__POS__;
  test ["ab"; "cd"] ["a"; "bc"; "d"] 0 ~__POS__;
  test [] ["a"; "bc"; "d"] (-1) ~__POS__;
  ()

let test_read_length () =
  Test.test "Bytes.Reader.read_length" @@ fun () ->
  let r = Bytes.Reader.of_string ~slice_length:2  "1234" in
  Test.int (Bytes.Reader.read_length r) 0;
  ignore (Bytes.Reader.read r);
  Test.int (Bytes.Reader.read_length r) 2;
  ignore (Bytes.Reader.read r);
  Test.int (Bytes.Reader.read_length r) 4;
  ignore (Bytes.Reader.read r); ignore (Bytes.Reader.read r);
  Test.int (Bytes.Reader.read_length r) 4;
  ()

let test_written_length () =
  Test.test "Bytes.Writer.written_length" @@ fun () ->
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer ~slice_length:2 b in
  Test.int (Bytes.Writer.written_length w) 0;
  Bytes.Writer.write_string w "1234";
  Test.int (Bytes.Writer.written_length w) 4;
  Bytes.Writer.write_string w "56";
  Test.int (Bytes.Writer.written_length w) 6;
  Bytes.Writer.write_string w "";
  Test.int (Bytes.Writer.written_length w) 6;
  Bytes.Writer.write_eod w;
  Test.int (Bytes.Writer.written_length w) 6;
  Test.string (Buffer.contents b) "123456";
  ()

let test_read_fun_eod () =
  Test.test "Bytes.Reader.t end of stream" @@ fun () ->
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
  Test.test "Bytes.Writer.t end of stream" @@ fun () ->
  let once = ref false in
  let write _slice = if !once then assert false else once := true in
  let w = Bytes.Writer.make write in
  Bytes.Writer.write w Bytes.Slice.eod;
  Bytes.Writer.write w Bytes.Slice.eod;
  Bytes.Writer.write w Bytes.Slice.eod;
  (Test.invalid_arg @@ fun () ->
   Bytes.Writer.write w (Bytes.Slice.of_bytes (bos "nooooo!")));
  ()

let test_reader_push_backs () =
  Test.test "Bytes.Reader.push_back" @@ fun () ->
  let r () = reader_of_list ["a"; "bb"; "ccc"] in
  let r0 = r () in
  eq_slice (Bytes.Reader.read r0) "a";
  let s = Bytes.Reader.read r0 in
  eq_slice s "bb";
  Test.int (Bytes.Reader.pos r0) 3;
  Bytes.Reader.push_back r0 s;
  Test.int (Bytes.Reader.pos r0) 1;
  eq_slice (Bytes.Reader.read r0) "bb";
  Test.int (Bytes.Reader.pos r0) 3;
  eq_slice (Bytes.Reader.read r0) "ccc";
  eq_eod (Bytes.Reader.read r0);
  let r1 = r () in
  Bytes.Reader.push_back r1 (Bytes.Slice.of_string "zz");
  Test.int (Bytes.Reader.pos r1) (-2);
  eq_slice (Bytes.Reader.read r1) "zz";
  Test.int (Bytes.Reader.pos r1) 0;
  eq_slice (Bytes.Reader.read r1) "a";
  Test.int (Bytes.Reader.pos r1) 1;
  Bytes.Reader.push_back r1 Bytes.Slice.eod;
  Test.int (Bytes.Reader.pos r1) 1;
  Test.string (Bytes.Reader.to_string r1) "bbccc";
  ()

let test_reader_skip_sub_limit () =
  Test.test "Bytes.Reader.{skip,sub,limit}" @@ fun () ->
  let r () = reader_of_list ["a"; "bb"; "ccc"] in
  let r0 = r () in
  Test.string (Bytes.Reader.to_string (Bytes.Reader.sub 2 r0)) "ab";
  Test.string (Bytes.Reader.to_string r0) "bccc";
  let r0 = r () in
  Test.string (Bytes.Reader.to_string (Bytes.Reader.sub 128 r0)) "abbccc";
  Test.string (Bytes.Reader.to_string r0) "";
  let r0 = r () in
  let lr0 = Bytes.Reader.limit 5 r0 in
  test_stream_error @@ (fun () -> Bytes.Reader.to_string lr0);
  eq_eod (Bytes.Reader.read lr0);
  let r0 = r () in
  let lr0 = Bytes.Reader.limit 6 r0 in
  Test.string (Bytes.Reader.to_string lr0) "abbccc" ~__POS__;
  let r0 = r () in
  let () = Bytes.Reader.skip 2 r0 in
  Test.string (Bytes.Reader.to_string r0) "bccc";
  let r0 = r () in
  let () = Bytes.Reader.skip 128 r0 in
  Test.string (Bytes.Reader.to_string r0) "";
  ()

let test_reader_append () =
  Test.test "Bytes.Reader.append" @@ fun () ->
  let r0 () = reader_of_list ["a"; "bb"; "ccc"] in
  let r1 () = reader_of_list ["d"; "ee"] in
  Test.string Bytes.Reader.(to_string (append (r0 ()) (r1 ()))) "abbcccdee";
  Test.string Bytes.Reader.(to_string (append (r0 ()) (empty ()))) "abbccc";
  Test.string Bytes.Reader.(to_string (append (empty ()) (r1 ()))) "dee";
  ()

let test_reader_sniff () =
  Test.test "Bytes.Reader.sniff" @@ fun () ->
  let r () = reader_of_list ["a"; "bb"; "ccc"] in
  let r0 = r () in
  Test.string (Bytes.Reader.sniff 2 r0) "ab";
  Test.string (Bytes.Reader.to_string r0) "abbccc";
  let r1 = r () in
  Test.string (Bytes.Reader.sniff 1 r1) "a";
  Test.string (Bytes.Reader.to_string r1) "abbccc";
  let r2 = r () in
  Test.string (Bytes.Reader.sniff 4 r2) "abbc";
  Test.string (Bytes.Reader.to_string r2) "abbccc";
  Test.string (Bytes.Reader.sniff (-1) r2) "";
  let r4 = reader_of_list ["a"; "bb"] in
  Test.string (Bytes.Reader.sniff 4 r4) "abb";
  Test.string (Bytes.Reader.to_string r4) "abb";
  let r5 = Bytes.Reader.empty () in
  Test.string (Bytes.Reader.sniff 4 r5) "";
  Test.string (Bytes.Reader.to_string r5) "";
  ()

let test_reader_of_slice () =
  Test.test "Bytes.Reader.of_slice" @@ fun () ->
  let r ~slice_length () =
    Bytes.Reader.of_slice ?slice_length (Bytes.Slice.of_string "bla")
  in
  eq_slices (r ~slice_length:None ()) ["bla"];
  eq_slices (r ~slice_length:(Some 1) ()) ["b"; "l"; "a"];
  eq_slices (r ~slice_length:(Some 2) ()) ["bl"; "a"];
  eq_slices (r ~slice_length:(Some 5) ()) ["bla"];
  ()

let test_writer_limit () =
  Test.test "Bytes.Writer.limit" @@ fun () ->
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer b in
  let lw = Bytes.Writer.limit 2 ~eod:true w in
  test_stream_error @@ (fun () -> Bytes.Writer.write_string lw "123");
  Test.string (Buffer.contents b) "12";
  Test.invalid_arg @@ (fun () -> Bytes.Writer.write_string lw "bla");
  Test.string (Buffer.contents b) "12";
  Bytes.Writer.write_eod lw;
  Test.string (Buffer.contents b) "12";
  Test.invalid_arg @@ (fun () -> Bytes.Writer.write_string w "1234");
  Test.string (Buffer.contents b) "12";
  let w = Bytes.Writer.of_buffer b in
  let lw = Bytes.Writer.limit 2 ~eod:false w in
  Bytes.Writer.write_string lw "12";
  Test.string (Buffer.contents b) "1212";
  Bytes.Writer.write_eod lw;
  Test.string (Buffer.contents b) "1212";
  let lw = Bytes.Writer.limit 2 ~eod:true w in
  Bytes.Writer.write_string lw "12";
  Test.string (Buffer.contents b) "121212";
  Bytes.Writer.write_eod lw;
  Test.string (Buffer.contents b) "121212";
  Test.invalid_arg @@ (fun () -> Bytes.Writer.write_string w "1234");
  ()


let main () =
  Test.main @@ fun () ->
  test_slice_make ();
  test_slice_sub ();
  test_slice_of_bytes ();
  test_slice_compare ();
  test_reader_reslice ();
  test_reader_compare ();
  test_read_length ();
  test_written_length ();
  test_read_fun_eod ();
  test_write_fun_eod ();
  test_reader_push_backs ();
  test_reader_skip_sub_limit ();
  test_reader_append ();
  test_reader_sniff ();
  test_reader_of_slice ();
  test_writer_limit ();
  ()


let () = if !Sys.interactive then () else exit (main ())
