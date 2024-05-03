(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let filter_stdio_with_bytes_reader ~in_size filter =
  let i = Bytes.Reader.of_in_channel ?slice_length:in_size In_channel.stdin in
  let o = Bytes.Writer.of_out_channel Out_channel.stdout in
  Bytes.Writer.write_reader ~eod:true o (filter i);
  i, o

let filter_stdio_with_bytes_writer ~out_size:osize filter =
  let i = Bytes.Reader.of_in_channel In_channel.stdin in
  let o = Bytes.Writer.of_out_channel ?slice_length:osize Out_channel.stdout in
  Bytes.Writer.write_reader ~eod:true (filter o) i;
  i, o

let decompress processor ~in_size ~out_size = match processor with
| `Reader ->
    let d = Bytesrw_zlib.Gzip.decompress_reads ?slice_length:out_size in
    filter_stdio_with_bytes_reader ~in_size d
| `Writer ->
    let d = Bytesrw_zlib.Gzip.decompress_writes ?slice_length:in_size in
    filter_stdio_with_bytes_writer ~out_size d

let compress processor ~in_size ~out_size = match processor with
| `Reader ->
    let c = Bytesrw_zlib.Gzip.compress_reads ?slice_length:out_size in
    filter_stdio_with_bytes_reader ~in_size c
| `Writer ->
    let c = Bytesrw_zlib.Gzip.compress_writes ?slice_length:in_size in
    filter_stdio_with_bytes_writer ~out_size c

let log_count i o =
  let i = Bytes.Reader.read_length i in
  let o = Bytes.Writer.written_length o in
  let pct = Float.to_int ((float o /. float i) *. 100.) in
  Printf.eprintf "i:%d o:%d o/i:%d%%\n%!" i o pct

let trip mode clevel processor in_size out_size show_count =
  try
    let i, o = match mode with
    | `Decompress -> decompress processor ~in_size ~out_size
    | `Compress -> compress processor ~in_size ~out_size
    in
    if show_count then log_count i o;
    Ok 0
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

open Cmdliner

let cmd =
  let doc = "Gzip (De)compression from stdin to stdout" in
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
        (Bytesrw_zlib.default_compression) (Bytesrw_zlib.best_compression)
    in
    Arg.(value & opt int (Bytesrw_zlib.default_compression) &
         info ["l"] ~doc ~docv:"LEVEL")
  in
  let show_count =
    let doc = "Show on $(b,stderr) final amount of bytes read and written." in
    Arg.(value & flag & info ["show-count"] ~doc)
  in
  Cmd.v (Cmd.info "gziptrip" ~version:"%%VERSION%%" ~doc) @@
  Term.(const trip $ mode $ clevel $ processor $
        in_size $ out_size $ show_count)

let main () = Cmd.eval_result' cmd
let () = if !Sys.interactive then () else exit (main ())
