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

(* Hashes *)

module Test_hash = struct
  type t =
    { data : string; sha_1 : string; sha_256 : string; sha_384 : string;
      sha_512 : string; sha3_224 : string; sha3_256 : string;
      sha3_384 : string; sha3_512 : string; }

  let t0 =
    { data = "";
      sha_1 =
        "da39a3ee5e6b4b0d3255bfef95601890afd80709";
      sha_256 =
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
      sha_384 =
        "38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da\
         274edebfe76f65fbd51ad2f14898b95b";
      sha_512 =
        "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce\
         47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e";
      sha3_224 =
        "6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7";
      sha3_256 =
        "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a";
      sha3_384 =
        "0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2a\
         c3713831264adb47fb6bd1e058d5f004";
      sha3_512 =
        "a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a6\
         15b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26" }

  let t1 =
    { data =
        "SHA-1 is used to generate a condensed representation of a \
         message called a message digest.";
      sha_1 =
        "a2ee13f265a9b1d7ce14e6b539a6371710e8e4cd";
      sha_256 =
        "aa97506c73d3b4207b7bc50faf8bf04c9918524db0ac827073e13067807815f9";
      sha_384 =
        "e7a9d7ff5f40240d0747d7d387dd59b3cf4275ba13f5465668b96c357aee6b47\
         20356901245d4d1339211ca4faff5fde";
      sha_512 =
        "08d45fb0542f32fd4db91767eaad372374f754c035e4920cae61e778d9118430\
         a0807f26aa8eab5b1f9425cabf96378694d0a0fadaad14ea30f5d1077d7e910b";
      sha3_224 =
        "53d97aa626ab7a58537357ab258ebc362cd0011878ae2ef1e6254eae";
      sha3_256 =
        "55dbc7fc98da06f1f9c5868b3f9538dcf9b103c6c194bbaceb6564d3ccd7c7ed";
      sha3_384 =
        "1dd35aafbbd9f600a1b56d63a0cc9975ee7997dc77cf7e41989c6c12ba65283f\
         ddb150cc58eee92ae561e1ad03e0a6be";
      sha3_512 =
        "ad9bc65501d47b56e4b7831ddd9ee2a51a71afacf39c174c24b87a256fd562c3\
         c58b2254970f3235b27597a6eb70648c5b574a759f0829bd72f9259bc844dc73" }

  let module' (module H : Bytesrw_crypto.Hash.T) testh =
    if not H.is_supported then Test.skip "%s is unsupported" H.id else
    let repeat n = Test.range ~kind:"slice_length" ~first:1 ~last:n in
    Test.string H.(to_hex (string t0.data)) (testh t0);
    Test.string H.(to_hex (string t1.data)) (testh t1);
    begin repeat 5 @@ fun n ->
      let r = Bytes.Reader.of_string ~slice_length:n t1.data in
      let r, st = H.reads r in
      let () = Bytes.Reader.discard r in
      Test.string H.(to_hex (value st)) (testh t1);
      let w = Bytes.Writer.of_buffer ~slice_length:n (Buffer.create 255) in
      let w, st = H.writes w in
      let () = Bytes.Writer.write_string w t1.data in
      let stc = H.State.copy st in
      Test.string H.(to_hex (value stc)) (testh t1);
      let () = Bytes.Writer.write_eod w in
      let stc = H.State.copy st in
      Test.string H.(to_hex (value stc)) (testh t1);
      let () = Bytes.Writer.write_eod w in
      Test.string H.(to_hex (value st)) (testh t1);
      ()
    end;
    let r, st = H.reads (Bytes.Reader.empty ()) in
    let () = Bytes.Reader.discard r in
    Test.string H.(to_hex (value st)) (testh t0);
    let w = Bytes.Writer.of_buffer ~slice_length:2 (Buffer.create 255) in
    let w, st = H.writes w in
    let () = Bytes.Writer.write_eod w in
    Test.string H.(to_hex (value st)) (testh t0);
    let h = H.(string t1.data) in
    Test.string H.(to_hex h) (testh t1);
    Test.holds (H.equal h (H.of_hex (H.to_hex h) |> Result.get_ok));
    Test.holds (H.equal h (H.of_binary_string (H.to_binary_string h)
                           |> Result.get_ok));
    ()
end

let test_hash_alg = (* Nothing tested here, just show supported hashes *)
  Test.test "Bytesrw_crypto.Hash.Algorithm.supported" @@ fun () ->
  let supported = Bytesrw_crypto.Hash.Algorithm.supported () in
  let pp_list = Fmt.(list ~sep:sp Bytesrw_crypto.Hash.Algorithm.pp) in
  Test.log "@[%a@]" pp_list supported;
  ()

let test_hash =
  let open Bytesrw_crypto in
  Test.test "Bytesrw_crypto.Hash" @@ fun () ->
  let alg = Hash.Algorithm.Sha_256 in
  let check_length = Hash.Algorithm.length alg in
  if not (Hash.Algorithm.is_supported alg)
  then Test.skip "%s is unsupported" (Hash.Algorithm.to_string alg) else
  let t = Hash.of_hex ~check_length Test_hash.t1.sha_256 |> Result.get_ok in
  Test.holds (Hash.verify_equal (Hash.string alg Test_hash.t1.data) t);
  ()

let test_hash_sha_1 =
  let open Bytesrw_crypto in
  Test.test "Bytesrw_crypto.Hash.Make(… Sha_1)" @@ fun () ->
  let module H = Hash.Make (struct let algorithm = Hash.Algorithm.Sha_1 end) in
  Test_hash.module' (module H) (fun t -> t.Test_hash.sha_1);
  ()

let test_sha_256 =
  Test.test "Bytesrw_md.Sha_256" @@ fun () ->
  Test_hash.module' (module Bytesrw_crypto.Sha_256) (fun t -> t.sha_256);
  ()

let test_sha_384 =
  let open Bytesrw_crypto in
  Test.test "Bytesrw_crypto.Hash.Make(… Sha_384)" @@ fun () ->
  let module H =
    Hash.Make (struct let algorithm = Hash.Algorithm.Sha_384 end) in
  Test_hash.module' (module H) (fun t -> t.Test_hash.sha_384);
  ()

let test_sha_512 =
  Test.test "Bytesrw_md.Sha_512" @@ fun () ->
  Test_hash.module' (module Bytesrw_crypto.Sha_512) (fun t -> t.sha_512);
  ()

let test_sha3_224 =
  let open Bytesrw_crypto in
  Test.test "Bytesrw_crypto.Hash.Make(… Sha3_224)" @@ fun () ->
  let module H =
    Hash.Make (struct let algorithm = Hash.Algorithm.Sha3_224 end) in
  Test_hash.module' (module H) (fun t -> t.Test_hash.sha3_224);
  ()

let test_sha3_256 =
  let open Bytesrw_crypto in
  Test.test "Bytesrw_crypto.Hash.Make(… Sha3_256)" @@ fun () ->
  let module H =
    Hash.Make (struct let algorithm = Hash.Algorithm.Sha3_256 end) in
  Test_hash.module' (module H) (fun t -> t.Test_hash.sha3_256);
  ()

let test_sha3_384 =
  let open Bytesrw_crypto in
  Test.test "Bytesrw_crypto.Hash.Make(… Sha3_384)" @@ fun () ->
  let module H =
    Hash.Make (struct let algorithm = Hash.Algorithm.Sha3_384 end) in
  Test_hash.module' (module H) (fun t -> t.Test_hash.sha3_384);
  ()

let test_sha3_512 =
  let open Bytesrw_crypto in
  Test.test "Bytesrw_crypto.Hash.Make(… Sha3_512)" @@ fun () ->
  let module H =
    Hash.Make (struct let algorithm = Hash.Algorithm.Sha3_512 end) in
  Test_hash.module' (module H) (fun t -> t.Test_hash.sha3_512);
  ()

(* Random *)

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
