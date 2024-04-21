(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let decompress in_size out_size =
  try
    (* FIXME in_size *)
    let compressed = Bytes.Reader.of_in_channel In_channel.stdin in
    let dec = Bytesrw_zstd.decompress ?slice_length:out_size compressed in
    let w = Bytes.Writer.of_out_channel Out_channel.stdout in
    Bytes.Writer.write_reader w dec;
    Ok 0
  with
  | Bytesrw_zstd.Error e -> Error e

let trip dec in_size out_size =
  if dec
  then decompress in_size out_size
  else Error "TODO"


open Cmdliner

let cmd =
  let doc = "Zstd (De)compression from stdin to stdout" in
  let decompress =
    let doc = "Decompress." in
    Arg.(value & flag & info ["d"] ~doc)
  in
  let in_size =
    let doc = "Input slices byte size." in
    Arg.(value & opt (some int) None & info ["in-size"] ~doc ~docv:"SIZE")
  in
  let out_size =
    let doc = "Output slices byte size." in
    Arg.(value & opt (some int) None & info ["out-size"] ~doc ~docv:"SIZE")
  in
  Cmd.v (Cmd.info "zstdtrip" ~version:"%%VERSION%%" ~doc)
    Term.(const trip $ decompress $ in_size $ out_size)



let main () = Cmd.eval_result' cmd

let () = if !Sys.interactive then () else exit (main ())
