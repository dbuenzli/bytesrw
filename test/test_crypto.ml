(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Bytesrw

(* These function pad the data with additional bytes. They are used
   to make sure the functions only mutate bytes in the slices. *)

let padded ?(fill = '\x00') s =
  let pad = String.make 1 fill in
  let last = String.length s in
  Bytes.Slice.of_string_or_eod ~first:1 ~last (pad ^ s ^ pad)

let padded_zero ?(fill = '\x00') n =
  Bytes.Slice.make_or_eod (Bytes.make (n + 2) fill) ~first:1 ~length:n

let test_padding ?__POS__:pos ?(fill = '\x00') s =
  (* Checks we didn't read outside the slice s *)
  Test.block ?__POS__:pos @@ fun () ->
  let bytes = Bytes.Slice.bytes s in
  Test.char (Bytes.get bytes 0) fill ~__POS__;
  Test.char (Bytes.get bytes (Bytes.length bytes - 1)) fill ~__POS__;
  ()

let test_clear =
  Test.test "Bytesrw_crypto.Clear.{slice,bytes,bigbytes}" @@ fun () ->
  let s = padded ~fill:'\x01' "abcd" in
  Bytesrw_crypto.Clear.slice s;
  Test.binary_string (Bytes.Slice.to_string s) (String.make 4 '\x00');
  test_padding ~fill:'\x01' s;
  let b = Bytes.of_string "efgh" in
  let bzero = Bytes.make 4 '\x00' in
  Test.(neq T.bytes) b bzero ~__POS__;
  Bytesrw_crypto.Clear.bytes b;
  Test.bytes b bzero ~__POS__;
  let bb = Bytesrw_crypto.Bigbytes.of_string "ijkl" in
  Test.(neq T.bytes) (Bytesrw_crypto.Bigbytes.to_bytes bb) bzero ~__POS__;
  Bytesrw_crypto.Clear.bigbytes bb;
  Test.bytes (Bytesrw_crypto.Bigbytes.to_bytes bb) bzero ~__POS__;
  ()

let test_verify =
  Test.test "Bytesrw_crypto.Verify.{equal_slices,equal_bigbytes}" @@ fun () ->
  let a0 = padded "" in
  let a3 = padded "abc" in
  let a4 = padded "abcd" and b4 = padded "abce" in
  Test.bool (Bytesrw_crypto.Verify.equal_slices a4 a4) true ~__POS__;
  Test.bool (Bytesrw_crypto.Verify.equal_slices b4 b4) true ~__POS__;
  Test.bool (Bytesrw_crypto.Verify.equal_slices a4 b4) false ~__POS__;
  Test.invalid_arg (fun () -> Bytesrw_crypto.Verify.equal_slices a4 a3)
    ~__POS__;
  Test.invalid_arg (fun () -> Bytesrw_crypto.Verify.equal_slices a3 a4)
    ~__POS__;
  Test.invalid_arg (fun () -> Bytesrw_crypto.Verify.equal_slices a0 a0)
    ~__POS__;
  test_padding a3; test_padding a4; test_padding b4;
  let a0 = Bytesrw_crypto.Bigbytes.of_string "" in
  let a3 = Bytesrw_crypto.Bigbytes.of_string "abc" in
  let a4 = Bytesrw_crypto.Bigbytes.of_string "abcd" in
  let b4 = Bytesrw_crypto.Bigbytes.of_string "abce" in
  Test.bool (Bytesrw_crypto.Verify.equal_bigbytes a4 a4) true ~__POS__;
  Test.bool (Bytesrw_crypto.Verify.equal_bigbytes b4 b4) true ~__POS__;
  Test.bool (Bytesrw_crypto.Verify.equal_bigbytes a4 b4) false ~__POS__;
  Test.invalid_arg (fun () -> Bytesrw_crypto.Verify.equal_bigbytes a4 a3)
    ~__POS__;
  Test.invalid_arg (fun () -> Bytesrw_crypto.Verify.equal_bigbytes a3 a4)
    ~__POS__;
  Test.invalid_arg (fun () -> Bytesrw_crypto.Verify.equal_bigbytes a0 a0)
    ~__POS__;
  ()

let test_random_set_random =
  Test.test "Bytesrw_crypto.Random.set_random" @@ fun () ->
  Bytesrw_crypto.Random.set_random Bytes.Slice.eod;
  let empty = padded_zero 0 in
  Test.holds (Bytes.Slice.is_eod empty);
  let () = Bytesrw_crypto.Random.set_random empty in
  Test.string (Bytes.Slice.to_string empty) "" ~__POS__;
  let some = padded_zero 444 in
  let before = Bytes.Slice.to_string some in
  let () = Bytesrw_crypto.Random.set_random some in
  let after = Bytes.Slice.to_string some in
  Test.(neq T.string) before after ~__POS__; (* with high probability *)
  test_padding some ~__POS__;
  ()

let test_random_reads =
  Test.test "Bytesrw_crypto.Random.reads" @@ fun () ->
  Test.invalid_arg ~__POS__ @@
  (fun () -> Bytesrw_crypto.Random.reads ~length:(-1) ());
  let zero = Bytes.Reader.to_string (Bytesrw_crypto.Random.reads ~length:0 ())in
  Test.binary_string zero "" ~__POS__;
  let once = Bytes.Reader.to_string (Bytesrw_crypto.Random.reads ~length:3 ())in
  Test.int (String.length once) 3 ~__POS__;
  let multi =
    let slice_length = 3 and length = 7 in
    let r = Bytesrw_crypto.Random.reads ~slice_length ~length () in
    Bytes.Reader.to_string r
  in
  Test.int (String.length multi) 7 ~__POS__;
  let infinite = Bytesrw_crypto.Random.reads ~slice_length:10 () in
  let first = Bytes.Reader.read infinite in
  Test.int (Bytes.Slice.length first) 10 ~__POS__;
  let second = Bytes.Reader.read infinite in
  Test.int (Bytes.Slice.length second) 10 ~__POS__;
  ()

let test_random_string_bytes =
  Test.test "Bytesrw_crypto.Random.{string,bytes}" @@ fun () ->
  Test.string "" (Bytesrw_crypto.Random.string 0) ~__POS__;
  Test.int 512 (String.length (Bytesrw_crypto.Random.string 512)) ~__POS__;
  Test.bytes Bytes.empty (Bytesrw_crypto.Random.bytes 0) ~__POS__;
  Test.int 512 (Bytes.length (Bytesrw_crypto.Random.bytes 512)) ~__POS__;
  ()

let main () = Test.main @@ fun () -> Test.autorun (); Gc.full_major ()
let () = if !Sys.interactive then () else exit (main ())
