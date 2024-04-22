(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let log fmt = Printf.eprintf (fmt ^^ "\n%!")
let tracer = Bytes.Slice.tracer ~ppf:Format.std_formatter

let main () =
  log "Testing Bytesrw";
  log "\027[32;1mSuccess!\027[m";
  0

let () = if !Sys.interactive then () else exit (main ())
