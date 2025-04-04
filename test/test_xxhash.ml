(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Bytesrw

let repeat n = Test.range ~kind:"slice_length" ~first:1 ~last:n

(* Test vectors *)

type t = { data : string; xxh3_64 : string; xxh3_128 : string; }
let t0 =
  { data = "";
    xxh3_64 = "2d06800538d394c2";
    xxh3_128 = "99aa06d3014798d86001c324468d497f"; }

let t1 =
  { data = "xxHash is an extremely fast non-cryptographic hash algorithm";
    xxh3_64 = "339d1954a9e06117";
    xxh3_128 = "07b0640ae1f202e6990373bfcc1e5c75" }

let t2 =
  { data = "abc\n";
    xxh3_64 = "079364cbfdf9f4cb";
    xxh3_128 = "158bbebef0c159c99914d27c01087efa"; }

let test_module (module H : Bytesrw_xxhash.Xxh3) testh =
  let hex = H.to_hex in
  assert (H.(hex (string t0.data) = testh t0));
  assert (H.(hex (string t1.data) = testh t1));
  assert (H.(hex (string t1.data) = testh t1));
  begin repeat 5 @@ fun n ->
    let r = Bytes.Reader.of_string ~slice_length:n t1.data in
    let r, st = H.reads r in
    let () = Bytes.Reader.discard r in
    assert (H.(hex (value st) = testh t1));
    let w = Bytes.Writer.of_buffer ~slice_length:n (Buffer.create 255) in
    let w, st = H.writes w in
    let () = Bytes.Writer.write_string w t1.data in
    assert (H.(hex (value st) = testh t1));
    let () = Bytes.Writer.write_eod w in
    assert (H.(hex (value st) = testh t1));
    let () = Bytes.Writer.write_eod w in
    assert (H.(hex (value st) = testh t1));
  end;
  let r, st = H.reads (Bytes.Reader.empty ()) in
  let () = Bytes.Reader.discard r in
  assert (H.(hex (value st) = testh t0));
  let w = Bytes.Writer.of_buffer ~slice_length:2 (Buffer.create 255) in
  let w, st = H.writes w in
  let () = Bytes.Writer.write_eod w in
  assert (H.(hex (value st) = testh t0));
  let f h =
    assert (H.equal h (H.of_hex (H.to_hex h) |> Result.get_ok));
    assert (H.equal h
              (H.of_binary_string (H.to_binary_string h) |> Result.get_ok));
  in
  f H.(string t0.data);
  f H.(string t1.data);
  f H.(string t2.data);
  ()

let test_xxh3_64 =
  Test.test "Bytesrw_xxhash.Xxh3_64" @@ fun () ->
  test_module (module Bytesrw_xxhash.Xxh3_64) (fun t -> t.xxh3_64);
  let h = Bytesrw_xxhash.Xxh3_64.(string t1.data) in
  assert (Printf.sprintf "%Lx" (Bytesrw_xxhash.Xxh3_64.to_uint64 h) =
          t1.xxh3_64);
  ()

let test_xxh3_128 =
  Test.test "Testing Bytesrw_xxhash.Xxh3_128" @@ fun () ->
  test_module (module Bytesrw_xxhash.Xxh3_128) (fun t -> t.xxh3_128);
  ()

(* Tests *)

let main () =
  Test.main @@ fun () ->
  Test.log "Using libxxhash %s" (Bytesrw_xxhash.version ());
  Test.autorun ();
  Gc.full_major ()

let () = if !Sys.interactive then () else exit (main ())
