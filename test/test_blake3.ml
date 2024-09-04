(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw
open B0_testing

let repeat n = Test.range ~kind:"slice_length" ~first:1 ~last:n

(* Test vectors *)

type t =
  { data : string;
    blake3 : string }

let t0 =
  { data = "";
    blake3 =
      "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262"; }

let t1 =
  { data =
      "BLAKE3 is based on an optimized instance of the established \
       hash function BLAKE2 and on the original Bao tree mode.";
    blake3 =
      "d352deef3f9b5aff803f7f2ab3aa4a15a0f21f4babce3534451057084155a280" }

(* Testing *)

let test_blake3 () =
  Test.test "Bytesrw_blake3.Blake3" @@ fun () ->
  let module H = Bytesrw_blake3.Blake3 in
  let testh t = t.blake3 and hex = H.to_hex in
  assert (H.(hex (string t0.data) = testh t0));
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
  let h = H.(string t1.data) in
  assert (hex h = testh t1);
  assert (H.equal h (H.of_hex (H.to_hex h) |> Result.get_ok));
  assert (H.equal h
            (H.of_binary_string (H.to_binary_string h) |> Result.get_ok));
  ()

let main () = Test.main @@ fun () ->
  Test.log "Using libblake3 %s" (Bytesrw_blake3.version ());
  test_blake3 ();
  Gc.full_major ()

let () = if !Sys.interactive then () else exit (main ())
