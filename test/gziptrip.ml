(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let stdin_reader unix ~slice_length = match unix with
| true -> Bytesrw_unix.bytes_reader_of_fd ?slice_length Unix.stdin
| false -> Bytes.Reader.of_in_channel ?slice_length In_channel.stdin

let stdout_writer unix ~slice_length = match unix with
| true -> Bytesrw_unix.bytes_writer_of_fd ?slice_length Unix.stdout
| false -> Bytes.Writer.of_out_channel ?slice_length Out_channel.stdout

let filter_stdio_with_reader_filter unix ~slice_length filter =
  let i = stdin_reader unix ~slice_length in
  let o = stdout_writer unix ~slice_length in
  Bytes.Writer.write_reader ~eod:true o (filter i);
  i, o

let filter_stdio_with_writer_filter unix ~slice_length filter =
  let i = stdin_reader unix ~slice_length in
  let o = stdout_writer unix ~slice_length in
  Bytes.Writer.write_reader ~eod:true (filter o) i;
  Bytes.Writer.write_eod o; (* Writer filters do not write eod *)
  i, o

let decompress processor unix ~slice_length = match processor with
| `Reader ->
    let d = Bytesrw_zlib.Gzip.decompress_reads () in
    filter_stdio_with_reader_filter unix ~slice_length d
| `Writer ->
    let d = Bytesrw_zlib.Gzip.decompress_writes () ~eod:true in
    filter_stdio_with_writer_filter unix ~slice_length d

let compress processor unix ~slice_length = match processor with
| `Reader ->
    let c = Bytesrw_zlib.Gzip.compress_reads () in
    filter_stdio_with_reader_filter unix ~slice_length c
| `Writer ->
    let c = Bytesrw_zlib.Gzip.compress_writes () ~eod:true in
    filter_stdio_with_writer_filter unix ~slice_length c

let log_count i o =
  let i = Bytes.Reader.read_length i in
  let o = Bytes.Writer.written_length o in
  let pct = Float.to_int ((float o /. float i) *. 100.) in
  Printf.eprintf "i:%d o:%d o/i:%d%%\n%!" i o pct

let trip mode clevel processor slice_length show_count unix =
  try
    let i, o = match mode with
    | `Decompress -> decompress processor unix ~slice_length
    | `Compress -> compress processor unix ~slice_length
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
    let r =
      `Reader, Arg.info ["reader"] ~doc:"Use a byte stream reader processor." in
    let w =
      `Writer, Arg.info ["writer"] ~doc:"Use a byte stream writer processor." in
    Arg.(value & vflag `Reader [r; w])
  in
  let slice_length =
    let doc = "IO byte slices size." in
    Arg.(value & opt (some int) None & info ["io-size"] ~doc ~docv:"SIZE")
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
  let unix =
    let doc = "Use OCaml Unix library I/O instead of Stdlib channels" in
    Arg.(value & flag & info ["unix-io"] ~doc)
  in
  Cmd.v (Cmd.info "gziptrip" ~version:"%%VERSION%%" ~doc) @@
  Term.(const trip $ mode $ clevel $ processor $
        slice_length $ show_count $ unix)

let main () = Cmd.eval_result' cmd
let () = if !Sys.interactive then () else exit (main ())
