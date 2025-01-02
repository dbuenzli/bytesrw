(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Bytesrw

let zero n =
  let bytes = Bytes.make (n + 2) '\x00' in
  Bytes.Slice.make_or_eod ~first:1 ~length:n bytes

let test_set_random =
  Test.test "Bytesrw_sysrandom.set_random" @@ fun () ->
  Bytesrw_sysrandom.set_random Bytes.Slice.eod;
  let empty = zero 0 in
  let () = Bytesrw_sysrandom.set_random empty in
  Test.string (Bytes.Slice.to_string empty) "" ~__POS__;
  let some = zero 444 in
  let before = Bytes.Slice.to_string some in
  let () = Bytesrw_sysrandom.set_random some in
  let after = Bytes.Slice.to_string some in
  Test.(neq T.string) before after ~__POS__; (* with high probability *)
  (* Test we wrote in slice bounds *)
  Test.char (Bytes.get (Bytes.Slice.bytes some) 0) '\x00';
  Test.char (Bytes.get (Bytes.Slice.bytes some) 445) '\x00';
  ()

let test_set_entropy =
  Test.test "Bytesrw_sysrandom.set_entropy" @@ fun () ->
  Bytesrw_sysrandom.set_entropy Bytes.Slice.eod;
  let empty = zero 0 in
  let () = Bytesrw_sysrandom.set_entropy empty in
  Test.string (Bytes.Slice.to_string empty) "" ~__POS__;
  let some = zero 256 in
  let before = Bytes.Slice.to_string some in
  let () = Bytesrw_sysrandom.set_entropy some in
  let after = Bytes.Slice.to_string some in
  Test.(neq T.string) before after ~__POS__; (* with high probability *)
  (* Test we wrote in slice bounds *)
  Test.char (Bytes.get (Bytes.Slice.bytes some) 0) '\x00';
  Test.char (Bytes.get (Bytes.Slice.bytes some) 257) '\x00';
  Test.invalid_arg (fun () -> Bytesrw_sysrandom.set_entropy (zero 257))
    ~__POS__;
  ()

let test_reads =
  Test.test "Bytesrw_sysrandom.reads" @@ fun () ->
  Test.invalid_arg ~__POS__ @@
  (fun () -> Bytesrw_sysrandom.reads ~length:(-1) ());
  let zero = Bytes.Reader.to_string (Bytesrw_sysrandom.reads ~length:0 ())in
  Test.binary_string zero "" ~__POS__;
  let once = Bytes.Reader.to_string (Bytesrw_sysrandom.reads ~length:3 ())in
  Test.int (String.length once) 3 ~__POS__;
  let multi =
    let slice_length = 3 and length = 7 in
    let r = Bytesrw_sysrandom.reads ~slice_length ~length () in
    Bytes.Reader.to_string r
  in
  Test.int (String.length multi) 7 ~__POS__;
  let infinite = Bytesrw_sysrandom.reads ~slice_length:10 () in
  let first = Bytes.Reader.read infinite in
  Test.int (Bytes.Slice.length first) 10 ~__POS__;
  let second = Bytes.Reader.read infinite in
  Test.int (Bytes.Slice.length second) 10 ~__POS__;
  ()

let test_string_bytes =
  Test.test "Bytesrw_sysrandom.{string,bytes}" @@ fun () ->
  Test.string "" (Bytesrw_sysrandom.string 0) ~__POS__;
  Test.int 512 (String.length (Bytesrw_sysrandom.string 512)) ~__POS__;
  Test.bytes Bytes.empty (Bytesrw_sysrandom.bytes 0) ~__POS__;
  Test.int 512 (Bytes.length (Bytesrw_sysrandom.bytes 512)) ~__POS__;
  ()

let main () = Test.main @@ fun () -> Test.autorun (); Gc.full_major ()
let () = if !Sys.interactive then () else exit (main ())
