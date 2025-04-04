(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Bytesrw

(* Bytesrw_crypto.Psa basic testing *)

module Psa = Bytesrw_crypto.Psa

let binary_string_of_hex s = Bytesrw_hex.to_binary_string s |> Result.get_ok
let slice_of_hex s = Bytes.Slice.of_string @@ (binary_string_of_hex s)

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

let eq_slice = Test.eq (module Bytes.Slice)
let neq_slice = Test.neq (module Bytes.Slice)
let test_status = Test.eq (module Psa.Status)
let test_success ?__POS__ st = test_status st Psa.success ?__POS__
let test_res = Test.result' ~error:(module Psa.Status)
let test_ires = test_res ~ok:Test.T.int

let test_psa_crypto_api_version =
  Test.test "Psa.crypto_api_version" @@ fun () ->
  let maj, min = Psa.crypto_api_version () in
  Test.log "PSA Crypto API: %d.%d" maj min;
  ()

let test_psa_alg =
  Test.test "Psa.Alg" @@ fun () ->
  (* Neither super interesting nor exhaustive *)
  Test.bool Psa.Alg.(is_hash none) false ~__POS__;
  Test.bool Psa.Alg.(is_hash xts) false ~__POS__;
  Test.bool Psa.Alg.(is_hash sha_512_224) true ~__POS__;
  Test.bool Psa.Alg.(is_cipher sha_512_224) false ~__POS__;
  Test.bool Psa.Alg.(is_cipher none) false ~__POS__;
  Test.bool Psa.Alg.(is_cipher xts) true ~__POS__;
  Test.bool Psa.Alg.(is_aead xts) false ~__POS__;
  Test.bool Psa.Alg.(is_aead none) false ~__POS__;
  Test.bool Psa.Alg.(is_aead xchacha20_poly1305) true ~__POS__;
  Test.bool Psa.Alg.(is_key_derivation xchacha20_poly1305) false ~__POS__;
  Test.bool Psa.Alg.(is_key_derivation none) false ~__POS__;
  Test.bool Psa.Alg.(is_key_derivation (hkdf sha_256)) true ~__POS__;
  Test.bool Psa.Alg.(is_sign (hkdf sha_256)) false ~__POS__;
  Test.bool Psa.Alg.(is_sign none) false ~__POS__;
  Test.bool Psa.Alg.(is_sign ecdsa_any) true ~__POS__;
  Test.bool Psa.Alg.(is_asymmetric_encryption ecdsa_any) false ~__POS__;
  Test.bool Psa.Alg.(is_asymmetric_encryption none) false ~__POS__;
  Test.bool Psa.Alg.(is_asymmetric_encryption rsa_pkcs1v15_crypt) true ~__POS__;
  Test.bool Psa.Alg.(is_key_agreement rsa_pkcs1v15_crypt) false ~__POS__;
  Test.bool Psa.Alg.(is_key_agreement none) false ~__POS__;
  ()

(* Test hashes *)

let test_psa_hash_single =
  Test.test "Psa.Hash.{compute,compare} (with SHA-256)" @@ fun () ->
  let vec0_sha_256 =
    "", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  in
  let vec1_sha_256 =
    "SHA-1 is used to generate a condensed representation of a \
     message called a message digest.",
    "aa97506c73d3b4207b7bc50faf8bf04c9918524db0ac827073e13067807815f9"
  in
  let alg = Psa.Alg.sha_256 in
  let hlen = Psa.Hash.length alg in
  Test.int hlen 32;
  let test_vec ?__POS__:pos (input, hex_hash) =
    Test.block ?__POS__:pos @@ fun () ->
    let bin_hash = Bytesrw_hex.to_binary_string hex_hash |> Result.get_ok' in
    let input = padded input in
    let hash = padded_zero hlen in
    test_ires (Psa.Hash.compute alg ~input ~hash) (Ok hlen) ~__POS__;
    Test.binary_string (Bytes.Slice.to_string hash) bin_hash ~__POS__;
    test_success (Psa.Hash.compare alg ~input ~hash:(padded bin_hash)) ~__POS__;
  in
  test_vec vec0_sha_256 ~__POS__;
  test_vec vec1_sha_256 ~__POS__;
  ()

(* Test MAC *)

let test_psa_mac_single =
  Test.test "Psa.Hmac.{compute,verify} (with HMAC-SHA-256) " @@ fun () ->
  (* Test case 2 of https://datatracker.ietf.org/doc/html/rfc4231#section-4 *)
  let key = "Jefe" in
  let input = Bytes.Slice.of_string "what do ya want for nothing?" in
  let tvec = slice_of_hex @@
    "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
  in
  let alg = Psa.Alg.(hmac sha_256) in
  let key, key_bits =
    let key_bits = String.length key * 8 in
    let atts = Psa.Key_attributes.init () in
    let usage = Psa.Key_usage.(sign_message + verify_message) in
    let () = Psa.set_key_usage_flags atts usage in
    let () = Psa.set_key_type atts Psa.Key_type.hmac in
    let () = Psa.set_key_algorithm atts alg in
    match Psa.import_key atts (Bytesrw_crypto.Bigbytes.of_string key) with
    | Ok id -> id, key_bits
    | Error e -> Test.failstop ~__POS__ "Cannot make key: %a" Psa.Status.pp e
  in
  let mac_len = Psa.Mac.length Psa.Key_type.hmac ~bits:key_bits alg in
  let mac = Bytes.Slice.of_bytes (Bytes.make mac_len '\x00') in
  test_ires (Psa.Mac.compute ~key alg ~input ~mac) (Ok mac_len) ~__POS__;
  Test.holds (Bytesrw_crypto.Verify.equal_slices mac tvec) ~__POS__;
  test_success (Psa.Mac.verify ~key alg ~input ~mac:tvec) ~__POS__;
  test_success (Psa.destroy_key key) ~__POS__;
  ()

(* Tests unauthenticated ciphers *)

let test_psa_cipher =
  Test.test "Psa.Cipher.* (with ChaCha20) " @@ fun () ->
  (* Test case from https://www.rfc-editor.org/rfc/rfc7539.html#section-2.4
     Note we can't use Psa.Cipher.decrypt with the test case because
     the counter is set to 0 in this case. We just round trip tplain
     for testing single part. *)
  let key = binary_string_of_hex @@
    "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
  in
  let tplain =
    "Ladies and Gentlemen of the class of '99: If I could offer you only \
     one tip for the future, sunscreen would be it."
  in
  let tcounter = 1l in
  let tnonce = binary_string_of_hex "000000000000004a00000000" in
  let tcipher = binary_string_of_hex @@
    "6e2e359a2568f98041ba0728dd0d6981\
     e97e7aec1d4360c20a27afccfd9fae0b\
     f91b65c5524733ab8f593dabcd62b357\
     1639d624e65152ab8f530c359f0861d8\
     07ca0dbf500d6a6156a38e088a22b65e\
     52bc514d16ccf806818ce91ab7793736\
     5af90bbf74a35be6b40b8eedf2785e42\
     874d"
  in
  let alg = Psa.Alg.stream_cipher in
  let key_type = Psa.Key_type.chacha20 in
  let key =
    let atts = Psa.Key_attributes.init () in
    let usage = Psa.Key_usage.(encrypt + decrypt) in
    let () = Psa.set_key_usage_flags atts usage in
    let () = Psa.set_key_type atts key_type in
    let () = Psa.set_key_algorithm atts alg in
    match Psa.import_key atts (Bytesrw_crypto.Bigbytes.of_string key) with
    | Ok id -> id
    | Error e -> Test.failstop ~__POS__ "Cannot make key: %a" Psa.Status.pp e
  in
  let () =
    (* Tests single-part encrypt/decrypt round-trip.
       This uses 0l for counter so we can't test against tcpher. *)
    let plain = Bytes.Slice.of_string tplain in
    let enc =
      let plain_length = Bytes.Slice.length plain in
      let n = Psa.Cipher.encrypt_output_size key_type alg ~plain_length in
      Bytes.Slice.of_bytes (Bytes.make n '\x00')
    in
    begin match Psa.Cipher.encrypt ~key alg ~plain ~cipher:enc with
    | Error e -> Test.fail "Couldn't encrypt: %a" Psa.Status.pp e
    | Ok len ->
        let cipher = Bytes.Slice.take len enc |> Option.get in
        let dec =
          let n =
            Psa.Cipher.decrypt_output_size key_type alg ~cipher_length:len
          in
          Bytes.Slice.of_bytes (Bytes.make n '\x00')
        in
        begin match Psa.Cipher.decrypt ~key alg ~cipher ~plain:dec with
        | Error e -> Test.fail "Couldn't decrypt: %a" Psa.Status.pp e
        | Ok len ->
            let plain' = Bytes.Slice.take len dec |> Option.get in
            Test.(neq T.string) ~__POS__
              (Bytes.Slice.to_string plain)
              (Bytes.Slice.to_string cipher);
            Test.string ~__POS__
              (Bytes.Slice.to_string plain')
              (Bytes.Slice.to_string plain);
        end;
    end;
  in
  let () =
    (* Theoretically this should work but
       https://github.com/Mbed-TLS/TF-PSA-Crypto/issues/162 *)
    (* Test multi-part decrypt/encrypt *)
    ignore (tcounter, tnonce, tcipher)
(*
    let op = Psa.Cipher.Operation.init () in
    let iv =
      (* See docs about ChaCha20 in
         https://arm-software.github.io/psa-api/crypto/1.2/api/ops/\
         ciphers.html#c.PSA_ALG_STREAM_CIPHER *)
      let counter =
        let b = Bytes.create 4 in Bytes.set_int32_le b 0 tcounter;
        Bytes.unsafe_to_string b
      in
      Bytes.Slice.of_string (counter ^ tnonce)
    in
    test_status (Psa.Cipher.decrypt_setup op ~key alg) Psa.success ~__POS__;
    Test.int (Bytes.Slice.length iv) 16;
    test_status (Psa.Cipher.set_iv op ~iv) Psa.success ~__POS__;
    let input = Bytes.Slice.of_string tcipher in
    let dec =
      let input_length = Bytes.Slice.length input in
      let n = Psa.Cipher.update_output_size key_type alg ~input_length in
      Bytes.Slice.of_bytes_or_eod (Bytes.make n '\x00')
    in
    begin match Psa.Cipher.update op ~input ~output:dec with
    | Error e -> Test.fail "Couldn't update: %a" Psa.Status.pp e
    | Ok len ->
        let p0 = Bytes.Slice.take len dec in
        let p0 = Option.fold ~none:"" ~some:Bytes.Slice.to_string p0 in
        let dec =
          let n = Psa.Cipher.finish_output_size key_type alg in
          Bytes.Slice.of_bytes_or_eod (Bytes.make n '\x00')
        in
        match Psa.Cipher.finish op ~output:dec with
        | Error e -> Test.fail "Couldn't finish: %a" Psa.Status.pp e
        | Ok len ->
            let p1 = Bytes.Slice.take len dec in
            let p1 = Option.fold ~none:"" ~some:Bytes.Slice.to_string p1 in
            let plain' = p0 ^ p1 in
            Test.binary_string ~__POS__ plain' tplain;
    end;
   *)
  in
  test_success (Psa.destroy_key key) ~__POS__;
  ()

(* Test AEAD *)

let test_psa_aead_single =
  Test.test "Psa.Aead.{encrypt,decrypt} (with ChaCha20-Poly1305)" @@ fun () ->
  (* Test case from
     https://datatracker.ietf.org/doc/html/rfc8439.html#section-2.8.2 *)
  let key = binary_string_of_hex
      "808182838485868788898a8b8c8d8e8f\
       909192939495969798999a9b9c9d9e9f"
  in
  let plain = Bytes.Slice.of_string
      "Ladies and Gentlemen of the class of '99: If I could offer you only \
       one tip for the future, sunscreen would be it."
  in
  let nonce = slice_of_hex "070000004041424344454647" in
  let ad = slice_of_hex "50515253c0c1c2c3c4c5c6c7" in
  let ciphertext = binary_string_of_hex
    "d31a8d34648e60db7b86afbc53ef7ec2\
     a4aded51296e08fea9e2b5a736ee62d6\
     3dbea45e8ca9671282fafb69da92728b\
     1a71de0a9e060b2905d6a5b67ecd3b36\
     92ddbd7f2d778b8c9803aee328091b58\
     fab324e4fad675945585808b4831d7bc\
     3ff4def08e4b7a9de576d26586cec64b\
     6116"
  in
  let tag = binary_string_of_hex "1ae10b594f09e26a7e902ecbd0600691" in
  let cipher = Bytes.Slice.of_string (ciphertext ^ tag) in
  let alg = Psa.Alg.chacha20_poly1305 in
  let key_type = Psa.Key_type.chacha20 in
  let key =
    let atts = Psa.Key_attributes.init () in
    let usage = Psa.Key_usage.(encrypt + decrypt) in
    let () = Psa.set_key_usage_flags atts usage in
    let () = Psa.set_key_type atts key_type in
    let () = Psa.set_key_algorithm atts alg in
    match Psa.import_key atts (Bytesrw_crypto.Bigbytes.of_string key) with
    | Ok id -> id
    | Error e -> Test.failstop ~__POS__ "Cannot make key: %a" Psa.Status.pp e
  in
  let dec =
    let cipher_length = Bytes.Slice.length cipher in
    let n = Psa.Aead.decrypt_output_size key_type alg ~cipher_length in
    Bytes.Slice.of_bytes (Bytes.make n '\x00')
  in
  begin match Psa.Aead.decrypt ~key alg ~nonce ~ad ~cipher ~plain:dec with
  | Error e -> Test.fail "Couldn't decrypt: %a" Psa.Status.pp e
  | Ok len ->
      let plain' = Bytes.Slice.take len dec |> Option.get in
      Test.string
        (Bytes.Slice.to_string plain') (Bytes.Slice.to_string plain) ~__POS__
  end;
  let enc =
    let plain_length = Bytes.Slice.length plain in
    let n = Psa.Aead.encrypt_output_size key_type alg ~plain_length in
    Bytes.Slice.of_bytes (Bytes.make n '\x00')
  in
  begin match Psa.Aead.encrypt ~key alg ~nonce ~ad ~plain ~cipher:enc with
  | Error e -> Test.fail "Couldn't encrypt: %a" Psa.Status.pp e
  | Ok len ->
      let cipher' = Bytes.Slice.take len enc |> Option.get in
      Test.string
        (Bytes.Slice.to_string cipher') (Bytes.Slice.to_string cipher) ~__POS__
  end;
  test_success (Psa.destroy_key key) ~__POS__;
  ()

let test_key_derivation =
  Test.test "Psa.Key_derivation.* (with PBKFD2-HMAC-SHA-256)" @@ fun () ->
  (* Test case from
     https://github.com/C2SP/wycheproof/blob/main/testvectors_v1/\
     pbkdf2_hmacsha256_test.json *)
  let password = slice_of_hex "5a30673349567272" in
  let salt = slice_of_hex "84bbd18de5ec10ff" in
  let iteration_count = 4096L in
  let derived_len = 42 in
  let derived =
    slice_of_hex
      "05fd57d1cc373fa9f37e1857ac1c0af8fbf635e139a42\
       f9dd25a4e4b4698ea13e943f42220384d32a272"
  in
  let result = padded_zero derived_len in
  let op = Psa.Key_derivation.Operation.init () in
  let alg = Psa.Alg.(pbkdf2_hmac sha_256) in
  test_success (Psa.Key_derivation.setup op alg) ~__POS__;
  test_success (Psa.Key_derivation.input_integer op Input_cost iteration_count);
  test_success (Psa.Key_derivation.input_bytes op Input_salt salt);
  test_success (Psa.Key_derivation.input_bytes op Input_password password);
  test_success (Psa.Key_derivation.output_bytes op result);
  Test.holds (Bytesrw_crypto.Verify.equal_slices result derived) ~__POS__;
(* Not supported by TF-PSA-Crypto 1.0.0
   https://github.com/Mbed-TLS/TF-PSA-Crypto/blob/8fa3370f747c5eae3c5004f47241cf45203c6a83/core/psa_crypto.c#L6571-L6580
  test_success (Psa.Key_derivation.verify_bytes op derived); *)
  ()

let test_sign =
  Test.test "Psa.Sign.{message,verify_message} (with ECDSA-secp256r1) " @@
  fun () ->
  let open Psa.Status.Syntax in
  let alg = Psa.Alg.(ecdsa sha_256) in
  let ecc_family = Psa.Ecc_family.secp_r1 in
  let key_type = Psa.Key_type.ecc_key_pair ecc_family in
  let key_type_pub = Psa.Key_type.ecc_public_key ecc_family in
  let key_bits = 256 in
  let () =
    (* Test case from
       https://github.com/C2SP/wycheproof/blob/main/testvectors_v1/\
       ecdsa_secp256r1_sha256_test.json *)
    let pub_key_der =
      Bytesrw_crypto.Bigbytes.of_bytes @@ Bytes.Slice.to_bytes @@
      (* As per PSA API key formats docs we need the "uncompressed" field *)
      slice_of_hex
        "042927b10512bae3eddcfe467828128bad2903269919f7086069c8c4df6c732838\
         c7787964eaac00e5921fb1498a60f4606766b3d9685001558d1a974e7341513e"
    in
    let pub_key =
      let attrs = Psa.Key_attributes.init () in
      let () = Psa.set_key_usage_flags attrs Psa.Key_usage.verify_message in
      let () = Psa.set_key_type attrs key_type_pub in
      let () = Psa.set_key_bits attrs key_bits in
      let () = Psa.set_key_algorithm attrs alg in
      match Psa.import_key attrs pub_key_der with
      | Error e -> Test.failstop "Couldn't import key: %a" Psa.Status.pp e
      | Ok kid -> kid
    in
    let msg = slice_of_hex "313233343030" in
    let signature =
      slice_of_hex @@ (* manually extracted r and s *)
      "2ba3a8be6b94d5ec80a6d9d1190a436effe50d85a1eee859b8cc6af9bd5c2e18" ^
      "4cd60b855d442f5b3c7b11eb6c4e0ae7525fe710fab9aa7c77a67f79e6fadd76"
    in
    test_success @@
    Psa.Sign.verify_message pub_key alg ~input:msg ~signature;
    test_success (Psa.destroy_key pub_key) ~__POS__;
  in
  let () =
    (* Create a private key *)
    let private_key =
      let attrs = Psa.Key_attributes.init () in
      let () = Psa.set_key_usage_flags attrs Psa.Key_usage.sign_message in
      let () = Psa.set_key_algorithm attrs alg in
      let () = Psa.set_key_type attrs key_type in
      let () = Psa.set_key_bits attrs key_bits in
      match Psa.generate_key attrs with
      | Error e -> Test.failstop "Couldn't generate key: %a" Psa.Status.pp e
      | Ok kid -> kid
    in
    (* Get its public key as a psa key *)
    let public_key =
      let pub_bin, length =
        let max_size =
          Psa.export_public_key_output_size key_type_pub ~bits:key_bits
        in
        let pub = Bytesrw_crypto.Bigbytes.create max_size in
        match Psa.export_public_key private_key pub with
        | Ok written -> pub, written
        | Error e -> Test.failstop "Could not export key: %a" Psa.Status.pp e
      in
      let attrs = Psa.Key_attributes.init () in
      let () = Psa.set_key_usage_flags attrs Psa.Key_usage.verify_message in
      let () = Psa.set_key_algorithm attrs alg in
      let () = Psa.set_key_type attrs key_type_pub in
      let () = Psa.set_key_bits attrs key_bits in
      match Psa.import_key attrs pub_bin ~length with
      | Error e -> Test.failstop "Couldn't import key: %a" Psa.Status.pp e
      | Ok kid -> kid
    in
    (* Sign some stuff *)
    let signature_size = Psa.Sign.output_size key_type ~bits:key_bits alg in
    let signature = Bytes.Slice.of_bytes (Bytes.make signature_size '\x00') in
    let rand_signature =
      let r = Bytes.Slice.of_bytes (Bytes.make signature_size '\x00') in
      test_success (Psa.generate_random r);
      r
    in
    let msg     = Bytes.Slice.of_string "The medium is the message" in
    let alt_msg = Bytes.Slice.of_string "The medium is the massage" in
    test_ires (Psa.Sign.message private_key alg ~input:msg ~signature)
      (Ok signature_size) ~__POS__;
    test_success
      (Psa.Sign.verify_message public_key alg ~input:msg ~signature) ~__POS__;
    test_status
      (Psa.Sign.verify_message public_key alg ~input:alt_msg ~signature)
      Psa.Error.invalid_signature ~__POS__;
    test_status
      (Psa.Sign.verify_message public_key alg ~input:msg
         ~signature:rand_signature)
      Psa.Error.invalid_signature ~__POS__;
    test_success (Psa.destroy_key private_key) ~__POS__;
    test_success (Psa.destroy_key public_key) ~__POS__;
  in
  ()

let test_asymmetric =
  Test.test "Psa.Asymmetric.{encrypt,decrypt} (with RSA-OAEP-SHA256)" @@
  fun () ->
  let alg = Psa.Alg.(rsa_oaep sha_256) in
  let key_bits = 1024 (* WARNING TOO SMALL, but we want a fast test suite *) in
  let key_type = Psa.Key_type.rsa_key_pair in
  let key_type_pub = Psa.Key_type.rsa_public_key in
  (* Create a private key *)
  let private_key =
    let attrs = Psa.Key_attributes.init () in
    let () = Psa.set_key_usage_flags attrs Psa.Key_usage.decrypt in
    let () = Psa.set_key_algorithm attrs alg in
    let () = Psa.set_key_type attrs key_type in
    let () = Psa.set_key_bits attrs key_bits in
    match Psa.generate_key attrs with
    | Error e -> Test.failstop "Couldn't generate key: %a" Psa.Status.pp e
    | Ok kid -> kid
  in
  (* Get its public key as a psa key *)
  let public_key =
    let pub_bin, length =
      let max_size =
        Psa.export_public_key_output_size key_type_pub ~bits:key_bits
      in
      let pub = Bytesrw_crypto.Bigbytes.create max_size in
      match Psa.export_public_key private_key pub with
      | Ok written -> pub, written
      | Error e -> Test.failstop "Could not export key: %a" Psa.Status.pp e
    in
    let attrs = Psa.Key_attributes.init () in
    let () = Psa.set_key_usage_flags attrs Psa.Key_usage.encrypt in
    let () = Psa.set_key_algorithm attrs alg in
    let () = Psa.set_key_type attrs key_type_pub in
    let () = Psa.set_key_bits attrs key_bits in
    match Psa.import_key attrs pub_bin ~length with
    | Error e -> Test.failstop "Couldn't import key: %a" Psa.Status.pp e
    | Ok kid -> kid
  in
  let plain = Bytes.Slice.of_string "Is it really that confidential ?" in
  let cipher, salt =
    let max_size =
      Psa.Asymmetric.encrypt_output_size
        Psa.Key_type.rsa_public_key ~bits:key_bits alg
    in
    let cipher = Bytes.Slice.of_bytes (Bytes.make max_size '\x00') in
    let salt = Bytes.Slice.of_bytes (Bytes.make max_size '\x00') in
    test_success (Psa.generate_random salt);
    cipher, Some salt
  in
  let cipher =
    match Psa.Asymmetric.encrypt public_key alg ~plain ~salt ~cipher with
    | Ok written -> Bytes.Slice.sub_or_eod cipher ~first:0 ~length:written
    | Error e -> Test.failstop "Could not encrypt: %a" Psa.Status.pp e
  in
  neq_slice cipher plain;
  let plain' =
    let max_size =
      Psa.Asymmetric.decrypt_output_size
        Psa.Key_type.rsa_key_pair ~bits:key_bits alg
    in
    Bytes.Slice.of_bytes (Bytes.make max_size '\x00')
  in
  let plain' =
    let plain = plain' in
    match Psa.Asymmetric.decrypt private_key alg ~cipher ~salt ~plain with
    | Ok written -> Bytes.Slice.sub_or_eod plain ~first:0 ~length:written
    | Error e -> Test.failstop "Could not decrypt: %a" Psa.Status.pp e
  in
  eq_slice plain plain';
  test_success (Psa.destroy_key private_key) ~__POS__;
  test_success (Psa.destroy_key public_key) ~__POS__;
  ()

let test_key_agreement =
  Test.test "Psa.Key_agreement.raw_agreement" @@ fun () ->
  let alg = Psa.Alg.ecdh in
  let key_family = Psa.Ecc_family.montgomery in
  let key_type = Psa.Key_type.ecc_key_pair key_family in
  let key_type_pub = Psa.Key_type.ecc_public_key key_family in
  let key_bits = 255 in (* Curve25519 *)
  let peer_key_pair () =
    (* Create a private key and gets its public key. *)
    let private_key =
      let attrs = Psa.Key_attributes.init () in
      Psa.set_key_usage_flags attrs Psa.Key_usage.derive;
      Psa.set_key_algorithm attrs alg;
      Psa.set_key_type attrs key_type;
      Psa.set_key_bits attrs key_bits;
      match Psa.generate_key attrs with
      | Error e -> Test.failstop "Couldn't generate key: %a" Psa.Status.pp e
      | Ok kid -> kid
    in
    (* Get its public key *)
    let max_size =
      Psa.export_public_key_output_size key_type_pub ~bits:key_bits
    in
    let pub = Bytesrw_crypto.Bigbytes.create max_size in
    match Psa.export_public_key private_key pub with
    | Error e -> Test.failstop "Could not export key: %a" Psa.Status.pp e
    | Ok length ->
        private_key,
        Bytes.Slice.of_bytes (Bytesrw_crypto.Bigbytes.to_bytes ~length pub)
  in
  let shared_secret ~private_key ~peer_key =
    let max_size = Psa.Key_agreement.raw_output_size key_type ~bits:key_bits in
    let output = Bytesrw_crypto.Bigbytes.make max_size ~fill:0x00 in
    match Psa.Key_agreement.raw_agreement alg ~private_key ~peer_key ~output
    with
    | Error e ->  Test.failstop "Could not agree: %a" Psa.Status.pp e
    | Ok length ->
        Bytes.Slice.of_bytes (Bytesrw_crypto.Bigbytes.to_bytes ~length output)
  in
  let alice_priv, alice_pub = peer_key_pair () in
  let bob_priv, bob_pub = peer_key_pair () in
  let alice_secret = shared_secret ~private_key:alice_priv ~peer_key:bob_pub in
  let bob_secret = shared_secret ~private_key:bob_priv ~peer_key:alice_pub in
  eq_slice alice_secret bob_secret;
  test_success (Psa.destroy_key alice_priv) ~__POS__;
  test_success (Psa.destroy_key bob_priv) ~__POS__;
  ()

(* Test random number generation *)

let test_psa_generate_random =
  Test.test "Psa.generate_random[_bigbytes]" @@ fun () ->
  test_success (Psa.generate_random Bytes.Slice.eod) ~__POS__;
  let empty = padded_zero 0 in
  Test.holds (Bytes.Slice.is_eod empty);
  test_success (Psa.generate_random empty) ~__POS__;
  Test.string (Bytes.Slice.to_string empty) "" ~__POS__;
  let some = padded_zero 444 in
  let before = Bytes.Slice.to_string some in
  test_success (Psa.generate_random some) ~__POS__;
  let after = Bytes.Slice.to_string some in
  Test.(neq T.string) before after ~__POS__; (* with high probability *)
  test_padding some ~__POS__;
  let before = Bytesrw_crypto.Bigbytes.create 512 in
  test_success (Psa.generate_random_bigbytes before);
  let after = Bytesrw_crypto.Bigbytes.create 512 in
  test_success (Psa.generate_random_bigbytes after);
  Test.(neq T.bigbytes) before after ~__POS__; (* with high probability *)
  ()

let main () = Test.main @@ fun () -> Test.autorun (); Gc.full_major ()
let () = if !Sys.interactive then () else exit (main ())
