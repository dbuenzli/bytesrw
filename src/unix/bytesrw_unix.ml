(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* Readers and writers. *)

let rec current_pos fd = match Unix.lseek fd 0 Unix.SEEK_CUR with
| exception Unix.Unix_error (ESPIPE, _, _) -> 0
| exception Unix.Unix_error (EINTR, _, _) -> current_pos fd
| pos -> if pos < 0 then 0 else pos

let bytes_reader_of_fd
    ?pos ?(slice_length = Bytes.Slice.unix_io_buffer_size) fd
  =
  let pos = match pos with Some pos -> pos | None -> current_pos fd in
  let b = Bytes.create (Bytes.Slice.check_length slice_length) in
  let rec read () = match Unix.read fd b 0 slice_length with
  | 0 -> Bytes.Slice.eod
  | count -> Bytes.Slice.make b ~first:0 ~length:count
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> read ()
  in
  Bytes.Reader.make ~pos ~slice_length read

let bytes_writer_of_fd
    ?pos ?(slice_length = Bytes.Slice.unix_io_buffer_size) fd
  =
  let pos = match pos with Some pos -> pos | None -> current_pos fd in
  let rec write = function
  | s when Bytes.Slice.is_eod s -> ()
  | s ->
      let b = Bytes.Slice.bytes s in
      let first = Bytes.Slice.first s and length = Bytes.Slice.length s in
      match Unix.single_write fd b first length with
      | count when count = length -> ()
      | count -> write (Option.get (Bytes.Slice.drop count s))
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> write s
  in
  Bytes.Writer.make ~pos ~slice_length write
