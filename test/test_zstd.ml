(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Format.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let assert_stream_error f =
  try f (); log "Expression did not raise Bytes.Stream.Error"; assert false with
  | Bytes.Stream.Error _ -> ()

let rec repeat n f =
  if n = 0 then () else begin
    (try f n with e -> log "Failing for slice_length %d" n; raise e);
    repeat (n - 1) f
  end

(* Test vectors *)

let a30_zstd = (* Note this compressed data has a checksum. *)
  "\x28\xb5\x2f\xfd\x04\x58\x45\x00\x00\x10\x61\x61\x01\x00\x0c\xc0\x02\x61\
   \x36\xf8\xbb", String.make 30 'a'

let b30_zstd = (* Note this compressed data has a checksum. *)
  "\x28\xb5\x2f\xfd\x04\x58\x45\x00\x00\x10\x62\x62\x01\x00\x0c\xc0\x02\xb3\
   \x56\x1f\x2e", String.make 30 'b'

let more = "moreatthedoor"

(* Tests *)

let test_decompress_reads () =
  log "Testing Bytesrw_zstd.decompress_reads";
  begin repeat 5 @@ fun n -> (* one frame *)
    let c = Bytes.Reader.of_string ~slice_length:n (fst a30_zstd) in
    let d = Bytesrw_zstd.decompress_reads () ~slice_length:n c in
    assert (Bytes.Reader.to_string d = snd a30_zstd)
  end;
  begin repeat 5 @@ fun n -> (* one frame with unexpected leftover data *)
    let c = Bytes.Reader.of_string ~slice_length:n (fst a30_zstd ^ more) in
    let d = Bytesrw_zstd.decompress_reads () ~slice_length:n c in
    assert_stream_error @@ fun () -> Bytes.Reader.to_string d
  end;
  begin repeat 5 @@ fun n -> (* one frame with expected leftover data *)
    let c = fst a30_zstd ^ more in
    let c = Bytes.Reader.of_string ~slice_length:n c in
    let d =
      Bytesrw_zstd.decompress_reads ~all_frames:false () ~slice_length:n c
    in
    assert (Bytes.Reader.to_string d = snd a30_zstd);
    assert (Bytes.Reader.pos c = String.length (fst a30_zstd));
    assert (Bytes.Reader.to_string c = more);
  end;
  begin repeat 5 @@ fun n -> (* two frames, one shot *)
    let data = (fst a30_zstd) ^ (fst b30_zstd) in
    let c = Bytes.Reader.of_string ~slice_length:n data in
    let d = Bytesrw_zstd.decompress_reads () ~slice_length:n c in
    assert (Bytes.Reader.to_string d = snd a30_zstd ^ snd b30_zstd)
  end;
  begin repeat 5 @@ fun n -> (* two frames, two shots *)
    let data = (fst a30_zstd) ^ (fst b30_zstd) in
    let c = Bytes.Reader.of_string ~slice_length:n data in
    let d =
      Bytesrw_zstd.decompress_reads ~all_frames:false () ~slice_length:n c
    in
    assert (Bytes.Reader.to_string d = snd a30_zstd);
    assert (Bytes.Reader.pos c = String.length (fst a30_zstd));
    let d =
      Bytesrw_zstd.decompress_reads ~all_frames:false () ~slice_length:n c
    in
    assert (Bytes.Reader.to_string d = snd b30_zstd);
    assert (Bytes.Reader.to_string c = "");
  end;
  ()

let test_decompress_writes () =
  log "Testing Bytesrw_zstd.decompress_writes";
  begin repeat 5 @@ fun n -> (* one frame *)
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let d = Bytesrw_zstd.decompress_writes () ~slice_length:n ~eod:true w in
    let c = Bytes.Reader.of_string ~slice_length:n (fst a30_zstd) in
    let () = Bytes.Writer.write_reader ~eod:true d c in
    assert (Buffer.contents b = snd a30_zstd)
  end;
  begin repeat 5 @@ fun n -> (* one with unexpected leftover *)
    let b = Buffer.create 255 in
    let w = Bytes.Writer.of_buffer ~slice_length:n b in
    let d = Bytesrw_zstd.decompress_writes () ~slice_length:n ~eod:true w in
    let c = Bytes.Reader.of_string ~slice_length:n ((fst a30_zstd) ^ more) in
    assert_stream_error @@ fun () -> Bytes.Writer.write_reader ~eod:true d c;
  end;
  ()

let test_compress_reads () =
  log "Testing Bytesrw_zstd.compress_reads";
  repeat 5 @@ fun n ->
  let data = snd a30_zstd in
  let d = Bytes.Reader.of_string ~slice_length:n data in
  let c = Bytesrw_zstd.compress_reads () ~slice_length:n d in
  let trip = Bytesrw_zstd.decompress_reads () ~slice_length:n c in
  assert (Bytes.Reader.to_string trip = data)

let test_compress_writes () =
  log "Testing Bytesrw_zstd.compress_writes";
  repeat 5 @@ fun n ->
  let data = snd a30_zstd in
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer ~slice_length:n b in
  let dw = Bytesrw_zstd.decompress_writes () ~slice_length:n ~eod:true w in
  let c = Bytesrw_zstd.compress_writes () ~slice_length:n ~eod:true dw in
  let rdata = Bytes.Reader.of_string ~slice_length:n data in
  let () = Bytes.Writer.write_reader ~eod:true c rdata in
  assert (Buffer.contents b = data)

let test_dictionary_support () =
  log "Testing dictionary support";
  repeat 5 @@ fun n ->
  let dict = "aaaaaaaa" in
  let data = "aaaaaaaabbbbbbbb" ^ "aaaaaaaa" ^ "aaaaaaaa" ^ "aaaaaaaa"in
  let cdict = Bytesrw_zstd.Cdict.of_binary_string dict in
  let ddict = Bytesrw_zstd.Ddict.of_binary_string dict in
  let datar = Bytes.Reader.of_string data ~slice_length:n in
  let c = Bytesrw_zstd.compress_reads ~dict:cdict () ~slice_length:n datar in
  let d = Bytesrw_zstd.decompress_reads ~dict:ddict () ~slice_length:n c in
  assert (Bytes.Reader.to_string d = data);
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
