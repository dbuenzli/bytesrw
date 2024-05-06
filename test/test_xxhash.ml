(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Format.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let assert_invalid_arg f =
  try f (); log "Expression did not raise Invalid_argument"; assert false with
  | Invalid_argument _ -> ()

let assert_stream_error f =
  try f (); log "Expression did not raise Bytes.Stream.Error"; assert false with
  | Bytes.Stream.Error _ -> ()

let rec repeat n f =
  if n = 0 then () else begin
    (try f n with e -> log "Failing for slice_length %d" n; raise e);
    repeat (n - 1) f
  end

(* Test vectors *)

type t =
  { data : string;
    xxh3_64 : int64;
    xxh3_128 : string; }

let t0 =
  { data = "";
    xxh3_64 = 0x2d06800538d394c2L;
    xxh3_128 =
      "\x99\xaa\x06\xd3\x01\x47\x98\xd8\x60\x01\xc3\x24\x46\x8d\x49\x7f" }

let t1 =
  { data = "xxHash is an extremely fast non-cryptographic hash algorithm";
    xxh3_64 = 0x339d1954a9e06117L;
    xxh3_128 =
      "\x07\xb0\x64\x0a\xe1\xf2\x02\xe6\x99\x03\x73\xbf\xcc\x1e\x5c\x75" }

let test_xxh3_64 () =
  log "Testing Bytesrw_xxhash.Xxh3_64";
  let module H = Bytesrw_xxhash.Xxh3_64 in
  let testh t = t.xxh3_64 and htest = H.to_uint64 in
  assert (H.(htest (string t0.data) = testh t0));
  assert (H.(htest (string t1.data) = testh t1));
  begin repeat 5 @@ fun n ->
    let r = Bytes.Reader.of_string ~slice_length:n t1.data in
    let r, st = H.reads r in
    let () = Bytes.Reader.discard r in
    assert (H.(htest (value st) = t1.xxh3_64));
    let w = Bytes.Writer.of_buffer ~slice_length:n (Buffer.create 255) in
    let w, st = H.writes w in
    let () = Bytes.Writer.write_string w t1.data in
    assert (H.(htest (value st) = testh t1));
    let () = Bytes.Writer.write_eod w in
    assert (H.(htest (value st) = testh t1));
    let () = Bytes.Writer.write_eod w in
    assert (H.(htest (value st) = testh t1));
  end;
  let r, st = H.reads (Bytes.Reader.empty ()) in
  let () = Bytes.Reader.discard r in
  assert (H.(htest (value st) = testh t0));
  let w = Bytes.Writer.of_buffer ~slice_length:2 (Buffer.create 255) in
  let w, st = H.writes w in
  let () = Bytes.Writer.write_eod w in
  assert (H.(htest (value st) = testh t0));
  let h = H.(string t1.data) in
  assert (H.to_uint64 h = testh t1);
  assert (H.equal h (H.of_hex (H.to_hex h) |> Result.get_ok));
  assert (H.equal h
            (H.of_binary_string (H.to_binary_string h) |> Result.get_ok));
  ()

let test_xxh3_128 () =
  log "Testing Bytesrw_xxhash.Xxh3_128";
  let module H = Bytesrw_xxhash.Xxh3_128 in
  let testh t = t.xxh3_128 and htest = H.to_binary_string in
  assert (H.(htest (string t0.data) = testh t0));
  assert (H.(htest (string t1.data) = testh t1));
  begin repeat 5 @@ fun n ->
    let r = Bytes.Reader.of_string ~slice_length:n t1.data in
    let r, st = H.reads r in
    let () = Bytes.Reader.discard r in
    assert (H.(htest (value st) = (testh t1)));
    let w = Bytes.Writer.of_buffer ~slice_length:n (Buffer.create 255) in
    let w, st = H.writes w in
    let () = Bytes.Writer.write_string w t1.data in
    assert (H.(htest (value st) = testh t1));
    let () = Bytes.Writer.write_eod w in
    assert (H.(htest (value st) = testh t1));
    let () = Bytes.Writer.write_eod w in
    assert (H.(htest (value st) = testh t1));
  end;
  let r, st = H.reads (Bytes.Reader.empty ()) in
  let () = Bytes.Reader.discard r in
  assert (H.(htest (value st) = testh t0));
  let w = Bytes.Writer.of_buffer ~slice_length:2 (Buffer.create 255) in
  let w, st = H.writes w in
  let () = Bytes.Writer.write_eod w in
  assert (H.(htest (value st) = testh t0));
  let h = H.(string t1.data) in
  assert (htest h = testh t1);
  assert (H.equal h (H.of_hex (H.to_hex h) |> Result.get_ok));
  assert (H.equal h
            (H.of_binary_string (H.to_binary_string h) |> Result.get_ok));
  ()

(* Tests *)

let main () =
  log "Testing Bytesrw_xxhash with libxxhash %s" (Bytesrw_xxhash.version ());
  test_xxh3_64 ();
  test_xxh3_128 ();
  Gc.full_major ();
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
