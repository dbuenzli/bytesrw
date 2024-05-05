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
    xxh3_64 : int64 }

let t0 =
  { data = "";
    xxh3_64 = 0x2d06800538d394c2L }

let t1 =
  { data = "xxHash is an extremely fast non-cryptographic hash algorithm";
    xxh3_64 = 0x339d1954a9e06117L; }

let test_xxh3_64 () =
  log "Testing Bytesrw_xxhash.Xxh3_64";
  assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (string t0.data) = t0.xxh3_64));
  assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (string t1.data) = t1.xxh3_64));
  begin repeat 5 @@ fun n ->
    let r = Bytes.Reader.of_string ~slice_length:n t1.data in
    let r, st = Bytesrw_xxhash.Xxh3_64.reads r in
    let () = Bytes.Reader.discard r in
    assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (value st) = t1.xxh3_64));
    let w = Bytes.Writer.of_buffer ~slice_length:n (Buffer.create 255) in
    let w, st = Bytesrw_xxhash.Xxh3_64.writes w in
    let () = Bytes.Writer.write_string w t1.data in
    assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (value st) = t1.xxh3_64));
    let () = Bytes.Writer.write_eod w in
    assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (value st) = t1.xxh3_64));
    let () = Bytes.Writer.write_eod w in
    assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (value st) = t1.xxh3_64));
  end;
  let r, st = Bytesrw_xxhash.Xxh3_64.reads (Bytes.Reader.empty ()) in
  let () = Bytes.Reader.discard r in
  assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (value st) = t0.xxh3_64));
  let w = Bytes.Writer.of_buffer ~slice_length:2 (Buffer.create 255) in
  let w, st = Bytesrw_xxhash.Xxh3_64.writes w in
  let () = Bytes.Writer.write_eod w in
  assert (Bytesrw_xxhash.Xxh3_64.(to_int64 (value st) = t0.xxh3_64));
  ()

(* Tests *)

let main () =
  log "Testing Bytesrw_xxhash with libxxhash %s" (Bytesrw_xxhash.version ());
  test_xxh3_64 ();
  Gc.full_major ();
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
