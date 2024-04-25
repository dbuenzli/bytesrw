(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Format.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter
let reader_of_list ss =
  Bytes.Reader.of_slice_seq (List.to_seq (List.map Bytes.Slice.of_string ss))

let test_guess_reader_encoding () =
  log "Test Bytesrw_utf.guess_reader_encoding";
  let test (s, exp) =
    let r = Bytes.Reader.of_string s in
    let g = Bytesrw_utf.guess_reader_encoding r in
    let pp_enc = Bytesrw_utf.pp_encoding in
    if exp <> g then begin
      log "expected: %a found: %a" pp_enc exp pp_enc g;
      assert false
    end else (assert (Bytes.Reader.to_string r = s));
  in
  (* This was taken from the uutf suite which also tested subsequent
     malformed data. *)
  (* UTF-8 guess *)
  test ("", `Utf_8);
  test ("\xEF", `Utf_8); (* malformed *)
  test ("\xEF\xBB", `Utf_8); (* malformed *)
  test ("\xEF\xBB\x00", `Utf_8); (* malformed *)
  test ("\xEF\xBB\xBF\xEF\xBB\xBF", `Utf_8);
  test ("\n\r\n", `Utf_8);
  test ("\n\x80\xEF\xBB\xBF\n", `Utf_8); (* malformed *)
  test ("\n\n\xEF\xBB\x00\n", `Utf_8); (* malformed *)
  test ("\n\xC8\x99", `Utf_8);
  test ("\xC8\x99\n", `Utf_8);
  test ("\xC8\x99\n\n", `Utf_8);
  test ("\xC8\x99\xC8\x99", `Utf_8);
  test ("\xC8\x99\xF0\x9F\x90\xAB", `Utf_8);
  test ("\xF0\x9F\x90\xAB\n", `Utf_8);
  (* UTF-16BE guess *)
  test ("\xFE\xFF\xDB\xFF\xDF\xFF\x00\x0A", `Utf_16be);
  test ("\xFE\xFF\xDB\xFF\x00\x0A\x00\x0A", `Utf_16be); (* malformed *)
  test ("\xFE\xFF\xDB\xFF\xDF", `Utf_16be); (* malformed *)
  test ("\x80\x81\xDB\xFF\xDF\xFF\xFE\xFF\xDF\xFF\xDB\xFF",
        `Utf_16be); (* malformed *)
  test ("\x80\x81\xDF\xFF\xDB\xFF\xFE", `Utf_16be); (* malformred *)
  test ("\x00\x0A", `Utf_16be);
  test ("\x00\x0A\xDB", `Utf_16be); (* malformed *)
  test ("\x00\x0A\xDB\xFF", `Utf_16be); (* malformed *)
  test ("\x00\x0A\xDB\xFF\xDF", `Utf_16be); (* malformed *)
  test ("\x00\x0A\xDB\xFF\xDF\xFF", `Utf_16be);
  test ("\x00\x0A\x00\x0A", `Utf_16be);
  (* UTF-16LE guess *)
  test ("\xFF\xFE\xFF\xDB\xFF\xDF\x0A\x00", `Utf_16le);
  test ("\xFF\xFE\xFF\xDB\x0A\x00\x0A\x00", `Utf_16le); (* malformed *)
  test ("\xFF\xFE\xFF\xDB\xDF", `Utf_16le); (* malformed *)
  test ("\x0A\x00", `Utf_16le);
  test ("\x0A\x00\xDB", `Utf_16le); (* malformed *)
  test ("\x0A\x00\xFF\xDB", `Utf_16le); (* malformed *)
  test ("\x0A\x00\xFF\xDB\xDF", `Utf_16le); (* malformed *)
  test ("\x0A\x00\xFF\xDB\xFF\xDF", `Utf_16le);
  test ("\x0A\x00\x0A\x00", `Utf_16le);
()


let main () =
  log "Testing Bytesrw_utf";
  test_guess_reader_encoding ();
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
