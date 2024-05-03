(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

 (* Uninterrupted reads and writes *)

let rec read fd b ~start:i ~length = try Unix.read fd b i length with
| Unix.Unix_error (Unix.EINTR, _, _) -> read fd b ~start:i ~length

let rec write fd s ~start:i ~length:l =
  let rec single_write fd s i l = try Unix.single_write fd s i l with
  | Unix.Unix_error (Unix.EINTR, _, _) -> single_write fd s i l
  in
  let bc = single_write fd s i l in
  if bc < l then write fd s ~start:(i + bc) ~length:(l - bc) else ()

let current_pos fd =
  match Unix.lseek fd 0 Unix.SEEK_CUR with
  | exception Unix.Unix_error (ESPIPE, _, _) -> 0
  | pos -> if pos < 0 then 0 else pos

(* Readers and writers. *)

let bytes_reader_of_fd
    ?pos ?(slice_length = Bytes.Slice.unix_io_buffer_size) fd
  =
  let pos = match pos with Some p -> p | None -> current_pos fd in
  let slice_length = Bytes.Slice.check_length slice_length in
  let b = Bytes.create slice_length in
  let read () =
    let count = read fd b ~start:0 ~length:(Bytes.length b) in
    if count = 0 then Bytes.Slice.eod else
    Bytes.Slice.make b ~first:0 ~length:count
  in
  Bytes.Reader.make ~pos ~slice_length read

let bytes_writer_of_fd
    ?pos ?(slice_length = Bytes.Slice.unix_io_buffer_size) fd
  =
  let pos = match pos with Some p -> p | None -> current_pos fd in
  let slice_length = Bytes.Slice.check_length slice_length in
  let write = function
  | s when Bytes.Slice.is_eod s -> ()
  | s ->
      let b = Bytes.Slice.bytes s in
      let start = Bytes.Slice.first s and length = Bytes.Slice.length s in
      write fd b ~start ~length
  in
  Bytes.Writer.make ~pos ~slice_length write
