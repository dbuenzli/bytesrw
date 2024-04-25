(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* XXX Generate Zstd_consts like we do with tsdl_consts.ml, this is
   very lazy and brittle. Values taken from the zstd.h *)

module Zstd_consts = struct
  let zstd_e_continue = 0
  let zstd_e_flush = 1
  let zstd_e_end = 2
  let zstd_c_compressionlevel = 100
  let zstd_c_windowlog = 101
  let zstd_c_checksumflag = 201
  let zstd_d_windowlogmax = 100
end

open Zstd_consts

(* Errors. Stubs raise [Failure] in case of error which we then
   turn into Bytes.Stream.Error with the following error. *)

type Bytes.Stream.error += Error of string

let format_error =
  let case msg = Error msg in
  let message = function Error msg -> msg | _ -> assert false in
  Bytes.Stream.make_format_error ~format:"zstd" ~case ~message

let error e = Bytes.Stream.error format_error e
let read_error r e = Bytes.Reader.read_error format_error r e
let write_error r e = Bytes.Writer.write_error format_error r e

(* Library parameters *)

external version : unit -> string = "ocaml_bytesrw_ZSTD_versionString"
external min_clevel : unit -> int = "ocaml_bytesrw_ZSTD_minCLevel"
external max_clevel : unit -> int = "ocaml_bytesrw_ZSTD_maxCLevel"
external default_clevel : unit -> int = "ocaml_bytesrw_ZSTD_defaultCLevel"
external cstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamInSize"
external cstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamOutSize"
external dstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamInSize"
external dstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamOutSize"

(* [Zbuf.t] values are used to communicate with C. It is a counter
   part to ZSTD_inBuffer and ZSTD_outBuffer on the OCaml side. *)

module Zbuf = struct
  type t = (* keep in sync with ocaml_zbuf_fields enum in C *)
    { mutable bytes : Bytes.t;
      mutable size : int;
      mutable pos : int; (* next read or write position *) }

  let make_empty () = { bytes = Bytes.empty; size = 0; pos = 0 }
  let make ?slice_length default_length =
    let size = match slice_length with
    | None -> default_length ()
    | Some size -> Bytes.Slice.check_length size
    in
    { bytes = Bytes.create size; size; pos = 0 }

  let make_for_writer w ~none = (* Adapts to [w]'s desire, if any. *)
    let slice_length = Bytes.Writer.slice_length w in
    make ?slice_length none

  let src_is_consumed buf = buf.pos >= buf.size
  let src_set_slice buf s =
    buf.bytes <- Bytes.Slice.bytes s;
    buf.size <- Bytes.Slice.first s + Bytes.Slice.length s;
    buf.pos <- Bytes.Slice.first s

  let dst_clear buf = buf.pos <- 0
  let dst_is_empty buf = buf.pos = 0
  let dst_is_full buf = buf.pos = buf.size
  let dst_to_slice buf = Bytes.Slice.make buf.bytes ~first:0 ~length:buf.pos
end

(* Decompression *)

type dctx (* Custom value holding a ZSTD_DCtx. We manually deallocate
             them when we know but they have a finalizer which accepts
             NULL pointers. *)

external create_dctx : unit -> dctx = "ocaml_bytesrw_ZSTD_createDCtx"
external free_dctx : dctx -> unit = "ocaml_bytesrw_ZSTD_freeDCtx"

external dctx_set_parameter : dctx -> int -> int -> unit =
  "ocaml_bytesrw_ZSTD_DCtx_setParameter"

external dctx_load_dictionary : dctx -> string -> unit =
  "ocaml_bytesrw_ZSTD_DCtx_loadDictionary"

external decompress_stream : dctx -> src:Zbuf.t -> dst:Zbuf.t -> bool =
  (* returns [true] at end of frames *)
  "ocaml_bytesrw_ZSTD_decompressStream"

module Dctx_params = struct
  type t =
    { window_log_max : int;
      unsafe_params : (int * int) list }

  let default =
    { window_log_max = 0;
      unsafe_params = [] }

  let make ?(init = default) ?window_log_max () =
    let window_log_max =
      Option.value ~default:init.window_log_max window_log_max
    in
    { window_log_max; unsafe_params = [] }

  let set ctx p =
    dctx_set_parameter ctx zstd_d_windowlogmax p.window_log_max;
    List.iter (fun (n, v) -> dctx_set_parameter ctx n v) p.unsafe_params

  let window_log_max p = p.window_log_max
  let unsafe_param n v p = { p with unsafe_params = (n, v) :: p.unsafe_params }
end

module Ddict = struct (* Could be expanded to full ZSTD_DDict support. *)
  type t = string
  let of_binary_string = Fun.id
end

let dctx_load_dictionary ctx dict = match dctx_load_dictionary ctx dict with
| exception Failure e -> free_dctx ctx; error e
| () -> ()

let make_dctx ?dict ?(params = Dctx_params.default) () =
  match create_dctx () with
  | exception Failure e -> error e
  | ctx ->
      Dctx_params.set ctx params;
      Option.iter (dctx_load_dictionary ctx) dict;
      ctx

let decompress error ctx ~src ~dst = match decompress_stream ctx ~src ~dst with
| is_eof -> is_eof | exception Failure e -> free_dctx ctx; error e

type decompress_state = Await | Flush

let err_unexpected_eod error = error "Unexpected end of compressed data"

let[@inline] not_flushed ~eof ~src ~dst =
  not (Zbuf.src_is_consumed src) || (Zbuf.dst_is_full dst && not eof)

let decompress_reads ?slice_length ?dict ?params r =
  let error = read_error r in
  let ctx = make_dctx ?dict ?params () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make ?slice_length dstream_out_size in
  let state = ref Await in
  let eof = ref false (* true on end of frames *) in
  let rec decode error ctx ~src ~dst =
    eof := decompress error ctx ~src ~dst;
    if Zbuf.dst_is_empty dst then (state := Await; read ()) else
    let slice = Zbuf.dst_to_slice dst in
    state := if not_flushed ~eof:!eof ~src ~dst then Flush else Await;
    Zbuf.dst_clear dst;
    slice
  and read () = match !state with
  | Flush -> decode error ctx ~src ~dst
  | Await ->
      match Bytes.Reader.read r with
      | slice when Bytes.Slice.is_eod slice ->
          free_dctx ctx; if !eof then slice else err_unexpected_eod error
      | slice ->
          Zbuf.src_set_slice src slice; decode error ctx ~src ~dst
  in
  let slice_length = Some dst.Zbuf.size in
  Bytes.Reader.make' ~parent:r ~slice_length read

let decompress_writes ?slice_length ?dict ?params w =
  let error = write_error w in
  let ctx = make_dctx ?dict ?params () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make_for_writer w ~none:dstream_out_size in
  let eof = ref false (* true on end of frames *) in
  let rec decode error ctx ~src ~dst =
    eof := decompress error ctx ~src ~dst;
    if Zbuf.dst_is_empty dst then () (* await *) else
    let slice = Zbuf.dst_to_slice dst in
    let not_flushed = not_flushed ~eof:!eof ~src ~dst in
    Zbuf.dst_clear dst;
    Bytes.Writer.write w slice;
    if not_flushed then decode error ctx ~src ~dst else () (* await *)
  and write = function
  | slice when Bytes.Slice.is_eod slice ->
      free_dctx ctx;
      if !eof then Bytes.Writer.write_eod w else err_unexpected_eod error
  | slice ->
      Zbuf.src_set_slice src slice; decode error ctx ~src ~dst
  in
  let slice_length = match slice_length with
  | None -> Some (dstream_in_size ()) | Some _ as l -> l
  in
  Bytes.Writer.make' ~parent:w ~slice_length write

(* Compression *)

type cctx (* Custom value holding a ZSTD_CCtx. We manually deallocate
             them when we know but they have a finalizer which accepts
             NULL pointers, if needed. *)

external create_cctx : unit -> cctx = "ocaml_bytesrw_ZSTD_createCCtx"
external free_cctx : cctx -> unit = "ocaml_bytesrw_ZSTD_freeCCtx"

external cctx_set_parameter : cctx -> int -> int -> unit =
  "ocaml_bytesrw_ZSTD_CCtx_setParameter"

external cctx_load_dictionary : cctx -> string -> unit =
  "ocaml_bytesrw_ZSTD_CCtx_loadDictionary"

external compress_stream2 :
  cctx -> src:Zbuf.t -> dst:Zbuf.t -> end_dir:int -> bool =
  (* returns [true] when end_dir is complete *)
  "ocaml_bytesrw_ZSTD_compressStream2"

module Cctx_params = struct
  type clevel = int
  type t =
    { checksum : bool;
      clevel : int;
      window_log : int;
      unsafe_params : (int * int) list }

  let default =
    { clevel = default_clevel (); checksum = false; window_log = 0;
      unsafe_params = [] }

  let make ?(init = default) ?checksum ?clevel ?window_log () =
    let checksum = Option.value ~default:init.checksum checksum in
    let clevel = Option.value ~default:init.clevel clevel in
    let window_log = Option.value ~default:init.window_log window_log in
    { clevel; checksum; window_log; unsafe_params = [] }

  let set ctx p =
    cctx_set_parameter ctx zstd_c_checksumflag (if p.checksum then 1 else 0);
    cctx_set_parameter ctx zstd_c_compressionlevel p.clevel;
    cctx_set_parameter ctx zstd_c_windowlog p.window_log;
    List.iter (fun (n, v) -> cctx_set_parameter ctx n v) p.unsafe_params

  let checksum p = p.checksum
  let clevel p = p.clevel
  let window_log p = p.window_log
  let unsafe_param n v p = { p with unsafe_params = (n, v) :: p.unsafe_params }
end

module Cdict = struct (* Could be expanded to full ZSTD_CDict support. *)
  type t = string
  let of_binary_string = Fun.id
end

let cctx_load_dictionary ctx dict = match cctx_load_dictionary ctx dict with
| exception Failure e -> free_cctx ctx; error e
| () -> ()

let make_cctx ?dict ?(params = Cctx_params.default) () =
  match create_cctx () with
  | exception Failure e -> error e
  | ctx ->
      Cctx_params.set ctx params;
      Option.iter (cctx_load_dictionary ctx) dict;
      ctx

let compress error ctx ~src ~dst ~end_dir =
  match compress_stream2 ctx ~src ~dst ~end_dir with
  | is_eodir -> is_eodir | exception Failure e -> free_cctx ctx; error e

type compress_state = Await | Flush | Flush_eod

let compress_reads ?slice_length ?dict ?params r =
  let error = read_error r in
  let ctx = make_cctx ?dict ?params () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make ?slice_length cstream_out_size in
  let state = ref Await in
  let eodir = ref false (* true when zstd_e_end has been encoded *) in
  let rec encode_e_end error ctx ~src ~dst =
    if !eodir then (free_cctx ctx; Bytes.Slice.eod) else begin
      eodir := compress error ctx ~src ~dst ~end_dir:zstd_e_end;
      if Zbuf.dst_is_empty dst then encode_e_end error ctx ~src ~dst else
      let slice = Zbuf.dst_to_slice dst in
      Zbuf.dst_clear dst;
      slice
    end
  in
  let rec encode error ctx ~src ~dst =
    ignore (compress error ctx ~src ~dst ~end_dir:zstd_e_continue);
    if Zbuf.dst_is_empty dst then (state := Await; read ()) else
    let slice = Zbuf.dst_to_slice dst in
    let flush = Zbuf.dst_is_full dst || not (Zbuf.src_is_consumed src)in
    state := if flush then Flush else Await;
    Zbuf.dst_clear dst;
    slice
  and read () = match !state with
  | Flush -> encode error ctx ~src ~dst
  | Flush_eod -> encode_e_end error ctx ~src ~dst
  | Await ->
      match Bytes.Reader.read r with
      | slice when Bytes.Slice.is_eod slice ->
          state := Flush_eod; encode_e_end error ctx ~src ~dst
      | slice ->
          Zbuf.src_set_slice src slice; encode error ctx ~src ~dst
  in
  let slice_length = Some dst.Zbuf.size in
  Bytes.Reader.make' ~parent:r ~slice_length read

let compress_writes ?slice_length ?dict ?params w =
  let error = write_error w in
  let ctx = make_cctx ?dict ?params () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make_for_writer w ~none:cstream_out_size in
  let rec encode_e_end error ctx ~src ~dst =
    let is_eodir = compress error ctx ~src ~dst ~end_dir:zstd_e_end in
    if not (Zbuf.dst_is_empty dst)
    then (Bytes.Writer.write w (Zbuf.dst_to_slice dst); Zbuf.dst_clear dst);
    if is_eodir then Bytes.Writer.write_eod w else
    encode_e_end error ctx ~src ~dst
  in
  let rec encode error ctx ~src ~dst =
    ignore (compress error ctx ~src ~dst ~end_dir:zstd_e_continue);
    if Zbuf.dst_is_empty dst then () (* await *) else
    let slice = Zbuf.dst_to_slice dst in
    let flush = Zbuf.dst_is_full dst || not (Zbuf.src_is_consumed src) in
    Zbuf.dst_clear dst;
    Bytes.Writer.write w slice;
    if flush then encode error ctx ~src ~dst else () (* await *)
  and write = function
  | slice when Bytes.Slice.is_eod slice -> encode_e_end error ctx ~src ~dst
  | slice -> Zbuf.src_set_slice src slice; encode error ctx ~src ~dst
  in
  let slice_length = match slice_length with
  | None -> Some (cstream_in_size ()) | Some _ as l -> l
  in
  Bytes.Writer.make' ~parent:w ~slice_length write
