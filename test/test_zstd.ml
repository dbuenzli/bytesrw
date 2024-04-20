(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let main () =
  print_endline ("libsztd " ^ Bytesrw_zstd.version ());
  print_endline "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
