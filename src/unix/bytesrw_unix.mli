(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** {!Unix} file descriptor byte stream readers and writers. *)

open Bytesrw

val bytes_reader_of_fd :
  ?pos:Bytes.Stream.pos -> ?slice_length:Bytes.Slice.length ->
  Unix.file_descr -> Bytes.Reader.t
(** [bytes_reader_of_fd fd] reads bytes from [fd] with {!Unix.read}
    and provides them as slices of maximal length [slice_length] (defaults to
    {!Bytesrw.Bytes.Slice.unix_io_buffer_size}). {!Bytesrw.Bytes.Slice.eod}
    is returned when {!Unix.read} returns [0].

    [pos] defaults to the [fd] position as determined by
    {!Unix.lseek}.  Reads are retried on {!Unix.EINTR} but both this
    function and the resulting reader may raise
    {!Unix.Unix_error}.

    {b Note.} The reader performs no ressource management. It is the client's
    duty to close the [fd]. *)

val bytes_writer_of_fd :
  ?pos:Bytes.Stream.pos -> ?slice_length:Bytes.Slice.length ->
  Unix.file_descr -> Bytes.Writer.t
(** [bytes_writer_of_fd fd] writes bytes to [fd] using
    {!Unix.single_write}. The hinted [slice_length] defaults to
    {!Bytesrw.Bytes.Slice.unix_io_buffer_size}. Writing
    {!Bytesrw.Bytes.Slice.eod} only terminates the writer, it makes
    no effect on [fd], it only terminates the writer.

    [pos] defaults to the [fd] position as determined by
    {!Unix.lseek}.  Writes are retried on {!Unix.EINTR} but both this
    function and the resulting writer may raise {!Unix.Unix_error}.

    {b Note.} The writer performs no ressource management. It is the client's
    duty to close the [fd]. *)
