(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let test =
  "\x28\xb5\x2f\xfd\x04\x58\x45\x00\x00\x10\x61\x61\x01\x00\x0c\xc0\x02\x61\
   \x36\xf8\xbb", String.make 30 'a'


let test_decompress () =
  print_endline "Testing decompression.";
  let compressed = Bytes.Reader.of_string ~slice_length:3 (fst test) in
  let decompressed = Bytesrw_zstd.decompress compressed in
  let dec = Bytes.Reader.to_string decompressed in
  Printf.printf "Decom: %S %d\n" dec (String.length dec);
  assert (snd test = dec)



let main () =
  print_endline ("libsztd " ^ Bytesrw_zstd.version ());
  test_decompress ();
  print_endline "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
