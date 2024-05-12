(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw
open B0_testing

let repeat = Test.repeat ~fail:"Failing for slice_length %d"

let test_stream_error f =
  let is_exn = function Bytes.Stream.Error _ -> true | _ -> false in
  Test.raises is_exn f

(* Test vectors *)

let a30_deflate =
  "\x4b\xc4\x0b\x00", String.make 30 'a'

let a30_zlib =
  "\x78\x9c\x4b\xc4\x0b\x00\xb0\x4f\x0b\x5f", String.make 30 'a'

let a_gz =
  "\x1f\x8b\x08\x08\x94\x58\x2d\x66\x00\x03\x61\x2e\x74\x78\x74\x00\x4b\x4c\
   \x84\x01\x00\xf0\xcd\x11\x4c\x0a\x00\x00\x00", String.make 10 'a'

let b_gz =
  "\x1f\x8b\x08\x08\x8c\x58\x2d\x66\x00\x03\x62\x2e\x74\x78\x74\x00\x4b\x4a\
   \x82\x01\x00\xf8\x4c\x2f\x42\x0a\x00\x00\x00", String.make 10 'b'

let more = "moreatthedoor"

(* Tests *)

let test_deflate_decompress_reads () =
  Test.test "Bytesrw_zlib.Deflate.decompress_reads" @@ fun () ->
  begin repeat 5 @@ fun n -> (* One stream *)
  let c = Bytes.Reader.of_string ~slice_length:n (fst a30_deflate) in
  let d = Bytesrw_zlib.Deflate.decompress_reads () c in
  assert (Bytes.Reader.to_string d = snd a30_deflate)
  end;
  begin repeat 5 @@ fun n -> (* One stream with unexpected leftover data *)
    let c = fst a30_deflate ^ more in
    let c = Bytes.Reader.of_string ~slice_length:n c in
    let d = Bytesrw_zlib.Deflate.decompress_reads () c in
    test_stream_error @@ fun () -> Bytes.Reader.to_string d
  end;
  begin repeat 5 @@ fun n -> (* One stream with expected leftover data *)
    let c = (fst a30_deflate) ^ more in
    let c = Bytes.Reader.of_string ~slice_length:n c in
    let d = Bytesrw_zlib.Deflate.decompress_reads ~leftover:true () c in
    assert (Bytes.Reader.to_string d = snd a30_deflate);
    assert (Bytes.Reader.pos c = String.length (fst a30_deflate));
    assert (Bytes.Reader.to_string c = more);
  end;
  begin repeat 5 @@ fun n -> (* One stream with expected leftover data but "" *)
    let c = (fst a30_deflate) in
    let c = Bytes.Reader.of_string ~slice_length:n c in
    let d = Bytesrw_zlib.Deflate.decompress_reads ~leftover:true () c in
    assert (Bytes.Reader.to_string d = snd a30_deflate);
    assert (Bytes.Reader.pos c = String.length (fst a30_deflate));
    assert (Bytes.Reader.to_string c = "");
  end;
  ()

let test_deflate_decompress_writes () =
  Test.test "Bytesrw_zlib.Deflate.decompress_writes" @@ fun () ->
  begin repeat 5 @@ fun n -> (* One stream. *)
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let d = Bytesrw_zlib.Deflate.decompress_writes ~eod:true () w in
    let c = Bytes.Reader.of_string ~slice_length:n (fst a30_deflate) in
    let () = Bytes.Writer.write_reader ~eod:true d c in
    assert (Buffer.contents b = snd a30_deflate);
  end;
  begin repeat 5 @@ fun n -> (* One stream with unexpected leftover *)
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let d = Bytesrw_zlib.Deflate.decompress_writes ~eod:true () w in
    let c = fst a30_deflate in
    let c = Bytes.Reader.of_string ~slice_length:n (c ^ c) in
    test_stream_error @@ fun () ->
    Bytes.Writer.write_reader ~eod:true d c
  end;
  ()

let test_deflate_compress_reads () =
  Test.test "Bytesrw_zlib.Deflate.compress_reads" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let data = snd a30_deflate in
    let d = Bytes.Reader.of_string ~slice_length:n data in
    let c = Bytesrw_zlib.Deflate.compress_reads () ~slice_length:n d in
    let trip = Bytesrw_zlib.Deflate.decompress_reads () ~slice_length:n c in
    assert (Bytes.Reader.to_string trip = data)
  end

let test_deflate_compress_writes () =
  Test.test "Bytesrw_zlib.Deflate.compress_writes" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let trip =
      Bytesrw_zlib.Deflate.compress_writes () ~slice_length:n ~eod:true @@
      Bytesrw_zlib.Deflate.decompress_writes () ~slice_length:n ~eod:true @@
      w
    in
    let data = snd a30_deflate in
    let r = Bytes.Reader.of_string ~slice_length:n data in
    let () = Bytes.Writer.write_reader ~eod:true trip r in
    assert (Buffer.contents b = data)
  end

let test_zlib_decompress_reads () =
  Test.test "Bytesrw_zlib.Zlib.decompress_reads" @@ fun () ->
  begin repeat 5 @@ fun n -> (* One stream *)
    let c = Bytes.Reader.of_string ~slice_length:n (fst a30_zlib) in
    let d = Bytesrw_zlib.Zlib.decompress_reads () c in
    assert (Bytes.Reader.to_string d = snd a30_zlib);
  end;
  begin repeat 5 @@ fun n -> (* One stream with expected leftover data *)
    let data = fst a30_zlib ^ more in
    let c = Bytes.Reader.of_string ~slice_length:n data in
    let d = Bytesrw_zlib.Zlib.decompress_reads ~leftover:true () c in
    assert (Bytes.Reader.to_string d = snd a30_zlib);
    assert (Bytes.Reader.pos c = String.length (fst a30_zlib));
    assert (Bytes.Reader.to_string c = more);
  end;
  ()

let test_zlib_decompress_writes () =
  Test.test "Bytesrw_zlib.Zlib.decompress_writes" @@ fun () ->
  begin repeat 5 @@ fun n -> (* One stream *)
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let d = Bytesrw_zlib.Zlib.decompress_writes () ~eod:true w in
    let c = Bytes.Reader.of_string ~slice_length:n (fst a30_zlib) in
    let () = Bytes.Writer.write_reader ~eod:true d c in
    assert (Buffer.contents b = snd a30_zlib);
  end

let test_zlib_compress_reads () =
  Test.test "Bytesrw_zlib.Zlib.compress_reads" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let data = snd a30_zlib in
    let d = Bytes.Reader.of_string ~slice_length:n data in
    let c = Bytesrw_zlib.Zlib.compress_reads () ~slice_length:n d in
    let trip = Bytesrw_zlib.Zlib.decompress_reads () ~slice_length:n c in
    assert (Bytes.Reader.to_string trip = data)
  end

let test_zlib_compress_writes () =
  Test.test "Bytesrw_zlib.Zlib.compress_writes" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let trip =
      Bytesrw_zlib.Zlib.compress_writes () ~slice_length:n ~eod:true @@
      Bytesrw_zlib.Zlib.decompress_writes () ~slice_length:n ~eod:true @@
      w
    in
    let data = snd a30_zlib in
    let r = Bytes.Reader.of_string ~slice_length:n data in
    let () = Bytes.Writer.write_reader ~eod:true trip r in
    assert (Buffer.contents b = data)
  end

let test_gzip_decompress_reads () =
  Test.test "Bytesrw_zlib.Gzip.decompress_reads" @@ fun () ->
  begin repeat 5 @@ fun n -> (* One member *)
    let c = Bytes.Reader.of_string ~slice_length:n (fst a_gz) in
    let d0 = Bytesrw_zlib.Gzip.decompress_reads () c in
    assert (Bytes.Reader.to_string d0 = snd a_gz);
  end;
  begin repeat 5 @@ fun n -> (* Two members, one shot *)
    let data = (fst a_gz) ^ (fst b_gz) in
    let c = Bytes.Reader.of_string ~slice_length:n data in
    let d0 = Bytesrw_zlib.Gzip.decompress_reads () c in
    assert (Bytes.Reader.to_string d0 = snd a_gz ^ snd b_gz);
  end;
  begin repeat 5 @@ fun n -> (* One member, leftover data *)
    let data = (fst a_gz) ^ more in
    let c = Bytes.Reader.of_string ~slice_length:n data in
    let d0 = Bytesrw_zlib.Gzip.decompress_reads ~all_members:false () c in
    assert (Bytes.Reader.to_string d0 = snd a_gz);
    assert (Bytes.Reader.pos c = String.length (fst a_gz));
    assert (Bytes.Reader.to_string c = more)
  end;
  begin repeat 5 @@ fun n -> (* One member, empty leftover data *)
    let c = Bytes.Reader.of_string ~slice_length:n (fst b_gz) in
    let d0 = Bytesrw_zlib.Gzip.decompress_reads ~all_members:false () c in
    assert (Bytes.Reader.to_string d0 = snd b_gz);
    assert (Bytes.Reader.pos c = String.length (fst b_gz));
    assert (Bytes.Reader.to_string c = "")
  end;
  begin repeat 5 @@ fun n -> (* Two members, two shots *)
    let cdata = (fst a_gz) ^ (fst b_gz) in
    let dlen = String.length cdata in
    let c = Bytes.Reader.of_string ~slice_length:n (cdata ^ more) in
    let d0 = Bytesrw_zlib.Gzip.decompress_reads ~all_members:false () c in
    assert (Bytes.Reader.to_string d0 = snd a_gz);
    assert (Bytes.Reader.pos c = String.length (fst a_gz));
    let d1 = Bytesrw_zlib.Gzip.decompress_reads ~all_members:false () c in
    assert (Bytes.Reader.to_string d1 = snd b_gz);
    assert (Bytes.Reader.pos c = dlen);
    assert (Bytes.Reader.to_string c = more);
  end;
  ()

let test_gzip_decompress_writes () =
  Test.test "Bytesrw_zlib.Gzip.decompress_writes" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let d = Bytesrw_zlib.Gzip.decompress_writes () ~eod:true w in
    let c = Bytes.Reader.of_string ~slice_length:n (fst a_gz) in
    let () = Bytes.Writer.write_reader ~eod:true d c in
    assert (Buffer.contents b = snd a_gz);
  end;
  begin repeat 5 @@ fun n ->
    let data = (fst a_gz) ^ (fst b_gz) in
    let res = (snd a_gz) ^ (snd b_gz) in
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let d = Bytesrw_zlib.Gzip.decompress_writes () ~eod:true w in
    let c = Bytes.Reader.of_string ~slice_length:n data in
    let () = Bytes.Writer.write_reader ~eod:true d c in
    assert (Buffer.contents b = res);
  end

let test_gzip_compress_reads () =
  Test.test "Bytesrw_zlib.Gzip.compress_reads" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let data = snd a_gz in
    let d = Bytes.Reader.of_string ~slice_length:n data in
    let c = Bytesrw_zlib.Gzip.compress_reads () ~slice_length:n d in
    let trip = Bytesrw_zlib.Gzip.decompress_reads () ~slice_length:n c in
    assert (Bytes.Reader.to_string trip = data)
  end

let test_gzip_compress_writes () =
  Test.test "Bytesrw_zlib.Gzip.compress_writes" @@ fun () ->
  begin repeat 5 @@ fun n ->
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let trip =
      Bytesrw_zlib.Gzip.compress_writes () ~slice_length:n ~eod:true @@
      Bytesrw_zlib.Gzip.decompress_writes () ~slice_length:n ~eod:true @@
      w
    in
    let data = snd a_gz in
    let r = Bytes.Reader.of_string ~slice_length:n data in
    let () = Bytes.Writer.write_reader ~eod:true trip r in
    assert (Buffer.contents b = data)
  end

let main () =
  Test.main @@ fun () ->
  Test.log "Using zlib %s" (Bytesrw_zlib.version ());
  test_deflate_decompress_reads ();
  test_deflate_decompress_writes ();
  test_deflate_compress_reads ();
  test_deflate_compress_writes ();
  test_zlib_decompress_reads ();
  test_zlib_decompress_writes ();
  test_zlib_compress_reads ();
  test_zlib_compress_writes ();
  test_gzip_decompress_reads ();
  test_gzip_decompress_writes ();
  test_gzip_compress_reads ();
  test_gzip_compress_writes ();
  Gc.full_major ()

let () = if !Sys.interactive then () else exit (main ())
