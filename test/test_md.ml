(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Bytesrw

let repeat = Test.repeat ~fail:"Failing for slice_length %d"

(* Test vectors *)

type t =
  { data : string; sha_1 : string; sha_256 : string; sha_384 : string;
    sha_512 : string; }

let t0 =
  { data = "";
    sha_1 = "da39a3ee5e6b4b0d3255bfef95601890afd80709";
    sha_256 =
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    sha_384 =
      "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da\
       274edebfe76f65fbd51ad2f14898b95b";
    sha_512 =
      "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce\
       47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"; }

let t1 =
  { data =
      "SHA-1 is used to generate a condensed representation of a \
       message called a message digest.";
    sha_1 = "a2ee13f265a9b1d7ce14e6b539a6371710e8e4cd";
    sha_256 =
      "aa97506c73d3b4207b7bc50faf8bf04c9918524db0ac827073e13067807815f9";
    sha_384 =
      "e7a9d7ff5f40240d0747d7d387dd59b3cf4275ba13f5465668b96c357aee6b47\
       20356901245d4d1339211ca4faff5fde";
    sha_512 =
      "08d45fb0542f32fd4db91767eaad372374f754c035e4920cae61e778d9118430\
       a0807f26aa8eab5b1f9425cabf96378694d0a0fadaad14ea30f5d1077d7e910b" }

let test_mod (module H : Bytesrw_md.Sha) testh =
  let hex = H.to_hex in
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

let test_sha_1 () =
  Test.test "Bytesrw_md.Sha_1" @@ fun () ->
  test_mod (module Bytesrw_md.Sha_1) (fun t -> t.sha_1);
  ()

let test_sha_256 () =
  Test.test "Bytesrw_md.Sha_256" @@ fun () ->
  test_mod (module Bytesrw_md.Sha_256) (fun t -> t.sha_256);
  ()

let test_sha_384 () =
  Test.test "Bytesrw_md.Sha_384" @@ fun () ->
  test_mod (module Bytesrw_md.Sha_384) (fun t -> t.sha_384);
  ()

let test_sha_512 () =
  Test.test "Bytesrw_md.Sha_512" @@ fun () ->
  test_mod (module Bytesrw_md.Sha_512) (fun t -> t.sha_512);
  ()

(* Tests *)

let main () =
  Test.main @@ fun () ->
  test_sha_1 ();
  test_sha_256 ();
  test_sha_384 ();
  test_sha_512 ();
  Gc.full_major ()

let () = if !Sys.interactive then () else exit (main ())
