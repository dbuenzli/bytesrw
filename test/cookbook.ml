(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Bytesrw

(* Applying filters to strings *)

let id s =
  let filters = Bytesrw_zstd.[compress_reads (); decompress_reads ()] in
  Bytes.Reader.filter_string filters s

let id s =
  let filters = Bytesrw_zstd.[decompress_writes (); compress_writes ()] in
  Bytes.Writer.filter_string filters s

(* Checksumming streams *)

let blake3_and_compress ~plain =
  try
    let plain, blake3 = Bytesrw_blake3.Blake3.reads plain in
    let comp = Bytesrw_zstd.compress_reads () plain in
    let comp = Bytes.Reader.to_string comp in
    Ok (comp, Bytesrw_blake3.Blake3.value blake3)
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let decompress_and_blake3 ~comp =
  try
    let plain = Bytesrw_zstd.decompress_reads () comp in
    let r, blake3 = Bytesrw_blake3.Blake3.reads plain in
    let s = Bytes.Reader.to_string r in
    Ok (s, Bytesrw_blake3.Blake3.value blake3)
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

(* Limiting streams *)

let limited_decompress ~quota ~comp =
  let buf = Buffer.create quota in
  try
    let plain = Bytesrw_zstd.decompress_reads () comp in
    let () = Bytes.Reader.add_to_buffer buf (Bytes.Reader.limit quota plain) in
    Ok (`Data (Buffer.contents buf))
  with
  |  Bytes.Stream.Error (Bytes.Stream.Limit _quota, _) ->
      Ok (`Quota_exceeded (Buffer.contents buf))
  |  Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

(* Tracing streams *)

let rtrace ~id r = Bytes.Reader.tap (Bytes.Slice.tracer ~id) r
let wtrace ~id w = Bytes.Writer.tap (Bytes.Slice.tracer ~id) w

(* Adding your own stream error *)

module Myformat : sig

  (** {1:errors Errors} *)

  type Bytesrw.Bytes.Stream.error +=
  | Error of string (** *)
  (** The type for [myformat] stream errors. *)

  (** {1:streams Streams} *)

  (* … *)
end = struct
  type Bytes.Stream.error += Error of string

  let format_error =
    let case msg = Error msg in
    let message = function Error msg -> msg | _ -> assert false in
    Bytes.Stream.make_format_error ~format:"myformat" ~case ~message

  let error e = Bytes.Stream.error format_error e
  let reader_error r e = Bytes.Reader.error format_error r e
  let writer_error w e = Bytes.Writer.error format_error w e
end
