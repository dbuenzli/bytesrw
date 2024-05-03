(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let filter_stdio_with_bytes_reader unix ~in_size filter =
  let i =
    if unix
    then Bytesrw_unix.bytes_reader_of_fd ?slice_length:in_size Unix.stdin
    else Bytes.Reader.of_in_channel ?slice_length:in_size In_channel.stdin
  in
  let o =
    if unix
    then Bytesrw_unix.bytes_writer_of_fd Unix.stdout
    else Bytes.Writer.of_out_channel Out_channel.stdout
  in
  Bytes.Writer.write_reader ~eod:true o (filter i);
  i, o

let filter_stdio_with_bytes_writer unix ~out_size:osize filter =
  let i =
    if unix
    then Bytesrw_unix.bytes_reader_of_fd Unix.stdin
    else Bytes.Reader.of_in_channel In_channel.stdin
  in
  let o =
    if unix
    then Bytesrw_unix.bytes_writer_of_fd ?slice_length:osize Unix.stdout
    else Bytes.Writer.of_out_channel ?slice_length:osize Out_channel.stdout
  in
  Bytes.Writer.write_reader ~eod:true (filter o) i;
  i, o

let decompress processor params unix ~in_size ~out_size = match processor with
| `Reader ->
    let d = Bytesrw_zstd.decompress_reads ?slice_length:out_size ~params in
    filter_stdio_with_bytes_reader unix ~in_size d
| `Writer ->
    let d = Bytesrw_zstd.decompress_writes ?slice_length:in_size ~params in
    filter_stdio_with_bytes_writer unix ~out_size d

let compress processor params unix ~in_size ~out_size = match processor with
| `Reader ->
    let c = Bytesrw_zstd.compress_reads ?slice_length:out_size ~params in
    filter_stdio_with_bytes_reader unix ~in_size c
| `Writer ->
    let c = Bytesrw_zstd.compress_writes ?slice_length:in_size ~params in
    filter_stdio_with_bytes_writer unix ~out_size c

let log_count i o =
  let i = Bytes.Reader.read_length i in
  let o = Bytes.Writer.written_length o in
  let pct = Float.to_int ((float o /. float i) *. 100.) in
  Printf.eprintf "i:%d o:%d o/i:%d%%\n%!" i o pct

let trip mode clevel no_checksum processor in_size out_size show_count unix =
  try
    let i, o = match mode with
    | `Decompress ->
        let params = Bytesrw_zstd.Dctx_params.make () in
        decompress processor params unix ~in_size ~out_size
    | `Compress ->
        let checksum = not no_checksum in
        let params = Bytesrw_zstd.Cctx_params.make ~clevel ~checksum () in
        compress processor params unix ~in_size ~out_size
    in
    if show_count then log_count i o;
    Ok 0
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

open Cmdliner

let cmd =
  let doc = "Zstd (De)compression from stdin to stdout" in
  let mode =
    let c = `Compress, Arg.info ["z"; "compress"] ~doc:"Compress." in
    let d = `Decompress, Arg.info ["d"; "decompress"] ~doc:"Decompress." in
    Arg.(value & vflag `Compress [c; d])
  in
  let processor =
    let r = `Reader, Arg.info ["reader"] ~doc:"Use a bytes reader processor." in
    let w = `Writer, Arg.info ["writer"] ~doc:"Use a bytes writer processor." in
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
  let show_count =
    let doc = "Show on $(b,stderr) final amount of bytes read and written." in
    Arg.(value & flag & info ["show-count"] ~doc)
  in
  let unix =
    let doc = "Use OCaml Unix library I/O instead of Stdlib channels" in
    Arg.(value & flag & info ["unix"] ~doc)
  in
  Cmd.v (Cmd.info "zstdtrip" ~version:"%%VERSION%%" ~doc) @@
  Term.(const trip $ mode $ clevel $ no_checksum $ processor $
        in_size $ out_size $ show_count $ unix)

let main () = Cmd.eval_result' cmd
let () = if !Sys.interactive then () else exit (main ())
