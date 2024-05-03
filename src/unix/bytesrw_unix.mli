(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Unix} bytes readers and writers.

    {b Note.} The reads and writes are retried on {!Unix.EINTR}. *)

open Bytesrw

val bytes_reader_of_fd :
  ?pos:int -> ?slice_length:Bytes.Slice.length ->
  Unix.file_descr -> Bytes.Reader.t
(** [bytes_reader_of_fd fd] reads bytes from [fd] with slices of maximal
    length [slice_length] (defaults to {!Bytes.Slice.unix_io_buffer_size}).
    [pos] defaults to the [fd] position as determined by {!Unix.lseek} *)


val bytes_writer_of_fd :
  ?pos:Bytes.Stream.pos -> ?slice_length:int -> Unix.file_descr ->
  Bytes.Writer.t
(** [bytes_writer_of_fd fd] writes bytes to [fd]. The hinted
    [slice_length] defaults to {!Bytes.Slice.unix_io_buffer_size}.
    [pos] defaults to the [fd] position as determined by {!Unix.lseek}. *)
