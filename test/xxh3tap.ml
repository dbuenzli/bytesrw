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

let xxh3_64_reader r =
  let i, hash = Bytesrw_xxhash.Xxh3_64.reads r in
  i, (fun () -> Bytesrw_xxhash.Xxh3_64.(to_hex (value hash)))

let xxh3_64_writer w =
  let w, hash = Bytesrw_xxhash.Xxh3_64.writes w in
  w, (fun () -> Bytesrw_xxhash.Xxh3_64.(to_hex (value hash)))

let xxh3_128_reader r =
  let i, hash = Bytesrw_xxhash.Xxh3_128.reads r in
  i, (fun () -> Bytesrw_xxhash.Xxh3_128.(to_hex (value hash)))

let xxh3_128_writer w =
  let w, hash = Bytesrw_xxhash.Xxh3_128.writes w in
  w, (fun () -> Bytesrw_xxhash.Xxh3_128.(to_hex (value hash)))

let hash_reader = function
| `Xxh3_64 -> xxh3_64_reader | `Xxh3_128 -> xxh3_128_reader

let hash_writer = function
| `Xxh3_64 -> xxh3_64_writer | `Xxh3_128 -> xxh3_128_writer

let sink hash processor unix ~slice_length = match processor with
| `Reader ->
    let i = stdin_reader unix ~slice_length in
    let i, get_hash = hash_reader hash i in
    let () = Bytes.Reader.discard i in
    i, get_hash ()
| `Writer ->
    let i = stdin_reader unix ~slice_length in
    let w, get_hash = hash_writer hash (Bytes.Writer.ignore ()) in
    let () = Bytes.Writer.write_reader w ~eod:true i in
    i, get_hash ()

let filter hash processor unix ~slice_length = match processor with
| `Reader ->
    let i = stdin_reader unix ~slice_length in
    let i, get_hash = hash_reader hash i in
    let o = stdout_writer unix ~slice_length in
    Bytes.Writer.write_reader ~eod:true o i;
    i, get_hash ()
| `Writer ->
    let i = stdin_reader unix ~slice_length in
    let o = stdout_writer unix ~slice_length in
    let o, get_hash = hash_writer hash o in
    Bytes.Writer.write_reader ~eod:true o i;
    i, get_hash ()

let log_count i = Printf.eprintf "i:%d\n%!" (Bytes.Reader.read_length i)

let tap mode hash processor slice_length show_count unix =
  try
    let i, hash = match mode with
    | `Sink -> sink hash processor unix ~slice_length
    | `Filter -> filter hash processor unix ~slice_length
    in
    if show_count then log_count i;
    Printf.eprintf "%s\n%!" hash;
    Ok 0
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

open Cmdliner

let cmd =
  let doc = "Rewrite stdin to stdout and report the xxh3-{64,128} of data" in
  let mode =
    let c = `Sink, Arg.info ["sink"] ~doc:"Only read stdin." in
    let d = `Filter, Arg.info ["filter"] ~doc:"Write stdin to stdout." in
    Arg.(value & vflag `Filter [c; d])
  in
  let processor =
    let r = `Reader, Arg.info ["reader"] ~doc:"Use a bytes reader tap." in
    let w = `Writer, Arg.info ["writer"] ~doc:"Use a bytes writer tap." in
    Arg.(value & vflag `Reader [r; w])
  in
  let slice_length =
    let doc = "IO byte slices size." in
    Arg.(value & opt (some int) None & info ["io-size"] ~doc ~docv:"SIZE")
  in
  let hash =
    let h64 = `Xxh3_64, Arg.info ["xxh3-64"] ~doc:"Use xxh3-64." in
    let h128 = `Xxh3_128, Arg.info ["xxh3-128"] ~doc:"Use xxh3-128." in
    Arg.(value & vflag `Xxh3_64 [h64; h128])
  in
  let show_count =
    let doc = "Show on $(b,stderr) final amount of bytes read." in
    Arg.(value & flag & info ["show-count"] ~doc)
  in
  let unix =
    let doc = "Use OCaml Unix library I/O instead of Stdlib channels" in
    Arg.(value & flag & info ["unix-io"] ~doc)
  in
  Cmd.v (Cmd.info "xxh3tap" ~version:"%%VERSION%%" ~doc) @@
  Term.(const tap $ mode $ hash $ processor $ slice_length $ show_count $ unix)

let main () = Cmd.eval_result' cmd
let () = if !Sys.interactive then () else exit (main ())
