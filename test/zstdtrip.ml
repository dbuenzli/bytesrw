(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let filter_stdio_with_bytes_reader ~in_size filter =
  let i = Bytes.Reader.of_in_channel ?slice_length:in_size In_channel.stdin in
  let o = Bytes.Writer.of_out_channel Out_channel.stdout in
  Bytes.Writer.write_reader ~eod:true o (filter i)

let filter_stdio_with_bytes_writer ~out_size:osize filter =
  let i = Bytes.Reader.of_in_channel In_channel.stdin in
  let o = Bytes.Writer.of_out_channel ?slice_length:osize Out_channel.stdout in
  Bytes.Writer.write_reader ~eod:true (filter o) i

let decompress processor params ~in_size ~out_size = match processor with
| `Reader ->
    let d = Bytesrw_zstd.decompress_reads ?slice_length:out_size ~params in
    filter_stdio_with_bytes_reader ~in_size d
| `Writer ->
    let d = Bytesrw_zstd.decompress_writes ?slice_length:in_size ~params in
    filter_stdio_with_bytes_writer ~out_size d

let compress processor params ~in_size ~out_size = match processor with
| `Reader ->
    let c = Bytesrw_zstd.compress_reads ?slice_length:out_size ~params in
    filter_stdio_with_bytes_reader ~in_size c
| `Writer ->
    let c = Bytesrw_zstd.compress_writes ?slice_length:in_size ~params in
    filter_stdio_with_bytes_writer ~out_size c

let trip mode clevel no_checksum processor in_size out_size =
  try match mode with
  | `Decompress ->
      let params = Bytesrw_zstd.Dctx_params.make () in
      decompress processor params ~in_size ~out_size; Ok 0
  | `Compress ->
      let checksum = not no_checksum in
      let params = Bytesrw_zstd.Cctx_params.make ~clevel ~checksum () in
      compress processor params ~in_size ~out_size; Ok 0
  with
  | Bytesrw_zstd.Error e -> Error e

open Cmdliner

let cmd =
  let doc = "Zstd (De)compression from stdin to stdout" in
  let mode =
    let c = `Compress, Arg.info ["z"; "compress"] ~doc:"Compress." in
    let d = `Decompress, Arg.info ["d"; "decompress"] ~doc:"Decompress." in
    Arg.(value & vflag `Compress [c; d])
  in
  let processor =
    let r = `Reader, Arg.info ["reader"] ~doc:"Use a bytes reader processor" in
    let w = `Writer, Arg.info ["writer"] ~doc:"Use a bytes writer processor" in
    Arg.(value & vflag `Reader [r; w])
  in
  let in_size =
    let doc = "Input slices byte size." in
    Arg.(value & opt (some int) None & info ["in-size"] ~doc ~docv:"SIZE")
  in
  let out_size =
    let doc = "Output slices byte size." in
    Arg.(value & opt (some int) None & info ["out-size"] ~doc ~docv:"SIZE")
  in
  let clevel =
    let doc =
      Printf.sprintf "Use compression level $(docv) (%d-%d)"
        (Bytesrw_zstd.min_clevel ()) (Bytesrw_zstd.max_clevel ())
    in
    Arg.(value & opt int (Bytesrw_zstd.default_clevel ()) &
         info ["l"] ~doc ~docv:"LEVEL")
  in
  let no_checksum =
    let doc = "Do not add integrity checksums" in
    Arg.(value & flag & info ["no-check"] ~doc)
  in
  Cmd.v (Cmd.info "zstdtrip" ~version:"%%VERSION%%" ~doc) @@
  Term.(const trip $ mode $ clevel $ no_checksum $ processor $
        in_size $ out_size)

let main () = Cmd.eval_result' cmd
let () = if !Sys.interactive then () else exit (main ())
