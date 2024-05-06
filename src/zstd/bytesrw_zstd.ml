(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* ZSTD_dParameter values,  keep order in sync with ocaml_zstd_d_parameter
   table in C stub *)

type d_parameter =
| D_windowlogmax | D_unsafe of int

(* ZSTD_cParameter values, keep order in sync with ocaml_zstd_c_parameter
   table in C stub *)

type c_parameter =
| C_compressionlevel | C_windowlog | C_checksumflag | C_unsafe of int

(* ZSTD_EndDirective values, keep order in sync with ocaml_zstd_end_directive
   table in C stub *)

type end_directive = E_continue | E_flush | E_end

(* [Zbuf.t] values are used to communicate buffers with C. It is a counter
   part to ZSTD_inBuffer and ZSTD_outBuffer on the OCaml side. *)

module Zbuf = struct
  type t = (* keep in sync with ocaml_zbuf_fields enum in C *)
    { mutable bytes : Bytes.t;
      mutable size : int; (* last read or write position + 1 *)
      mutable pos : int; (* next read or write position *) }

  let make_empty () = { bytes = Bytes.empty; size = 0; pos = 0 }
  let make size =
    { bytes = Bytes.create (Bytes.Slice.check_length size); size; pos = 0 }

  let src_is_consumed buf = buf.pos >= buf.size
  let src_rem buf = buf.size - buf.pos
  let src_set_slice buf s =
    buf.bytes <- Bytes.Slice.bytes s;
    buf.size <- Bytes.Slice.first s + Bytes.Slice.length s;
    buf.pos <- Bytes.Slice.first s

  let src_to_slice_or_eod buf =
    let src_rem = src_rem buf in
    if src_rem = 0 then Bytes.Slice.eod else
    Bytes.Slice.make buf.bytes ~first:buf.pos ~length:src_rem

  let dst_clear buf = buf.pos <- 0
  let dst_is_empty buf = buf.pos = 0
  let dst_is_full buf = buf.pos = buf.size
  let dst_to_slice buf = Bytes.Slice.make buf.bytes ~first:0 ~length:buf.pos
end

(* Errors. Stubs raise [Failure] in case of error which we turn into
   Bytes.Stream.Error with the following error. *)

type Bytes.Stream.error += Error of string

let format_error =
  let case msg = Error msg in
  let message = function Error msg -> msg | _ -> assert false in
  Bytes.Stream.make_format_error ~format:"zstd" ~case ~message

let error e = Bytes.Stream.error format_error e
let reader_error r e = Bytes.Reader.error format_error r e
let writer_error w e = Bytes.Writer.error format_error w e

(* Library parameters *)

external version : unit -> string = "ocaml_bytesrw_ZSTD_versionString"
external min_clevel : unit -> int = "ocaml_bytesrw_ZSTD_minCLevel"
external max_clevel : unit -> int = "ocaml_bytesrw_ZSTD_maxCLevel"
external default_clevel : unit -> int = "ocaml_bytesrw_ZSTD_defaultCLevel"
external cstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamInSize"
external cstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_CStreamOutSize"
external dstream_in_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamInSize"
external dstream_out_size : unit -> int = "ocaml_bytesrw_ZSTD_DStreamOutSize"

(* Decompression *)

type dctx (* Custom value holding a ZSTD_DCtx. We manually deallocate
  them when we know but they have a finalizer which accepts NULL pointers. *)

external create_dctx : unit -> dctx = "ocaml_bytesrw_ZSTD_createDCtx"
external free_dctx : dctx -> unit = "ocaml_bytesrw_ZSTD_freeDCtx"

external dctx_set_parameter : dctx -> d_parameter -> int -> unit =
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

  let make ?(init = default) ?window_log_max:wl () =
    let window_log_max = Option.value ~default:init.window_log_max wl in
    { window_log_max; unsafe_params = [] }

  let set ctx p = (* raises Failure *)
    let set_unsafe (n, v) = dctx_set_parameter ctx (D_unsafe n) v in
    dctx_set_parameter ctx D_windowlogmax p.window_log_max;
    List.iter set_unsafe p.unsafe_params

  let window_log_max p = p.window_log_max
  let unsafe_param n v p = { p with unsafe_params = (n, v) :: p.unsafe_params }
end

module Ddict = struct (* Could be expanded to full ZSTD_DDict support. *)
  type t = string
  let of_binary_string = Fun.id
end

let make_dctx ?dict ?(params = Dctx_params.default) () =
  match create_dctx () with
  | exception Failure e -> error e
  | ctx ->
      try
        Dctx_params.set ctx params;
        Option.iter (dctx_load_dictionary ctx) dict;
        ctx
      with Failure e -> free_dctx ctx; error e

type decompress_eof_action = Stop | Next
type decompress_state =
| Await | Eod | Eof of { leftover : Bytes.Slice.t } | Flush

let err_unexp_eod = "Unexpected end of compressed data"

let decompress_reads
    ?(all_frames = true) ?dict ?params ()
    ?pos ?(slice_length = dstream_out_size ()) r
  =
  let eof_action = if all_frames then Next else Stop in
  let ctx = make_dctx ?dict ?params () in
  let src = Zbuf.make_empty () and dst = Zbuf.make slice_length in
  let error = reader_error r in
  let state = ref Await in
  (* The following invariant must hold. [free_dctx ctx] is only ever
     called after [state] becomes Eod. This state is sticky and any
     read in this state returns [Bytes.Slice.eod]. *)
  let rec decompress ~error ctx ~src ~dst =
    match decompress_stream ctx ~src ~dst with
    | exception Failure e -> state := Eod; free_dctx ctx; error e
    | eof ->
        state := begin
          if eof then Eof { leftover = Zbuf.src_to_slice_or_eod src } else
          if not (Zbuf.src_is_consumed src) || (Zbuf.dst_is_full dst) then Flush
          else Await
        end;
        if Zbuf.dst_is_empty dst
        then read ()
        else let slice = Zbuf.dst_to_slice dst in (Zbuf.dst_clear dst; slice)
  and await r ~error ctx ~src ~dst =
    let slice = Bytes.Reader.read r in
    if Bytes.Slice.is_eod slice
    then (state := Eod; free_dctx ctx; error err_unexp_eod)
    else (Zbuf.src_set_slice src slice; decompress ~error ctx ~src ~dst)
  and eof_stop ~leftover r ctx =
    state := Eod; free_dctx ctx;
    Bytes.Reader.push_back r leftover;
    Bytes.Slice.eod;
  and eof_next ~leftover r ~error ctx ~src ~dst =
    let slice = match Bytes.Slice.is_eod leftover with
    | true -> Bytes.Reader.read r | false -> leftover
    in
    if Bytes.Slice.is_eod slice
    then (state := Eod; free_dctx ctx; slice) else
    (Zbuf.src_set_slice src slice; decompress ~error ctx ~src ~dst)
  and read () = match !state with
  | Await -> await r ~error ctx ~src ~dst
  | Flush -> decompress ~error ctx ~src ~dst
  | Eof { leftover } ->
      begin match eof_action with
      | Stop -> eof_stop ~leftover r ctx
      | Next -> eof_next ~leftover r ~error ctx ~src ~dst
      end
  | Eod -> Bytes.Slice.eod
  in
  Bytes.Reader.make ?pos ~slice_length read

let decompress_writes
    ?dict ?params () ?pos ?(slice_length = dstream_in_size ()) ~eod w
  =
  let ctx = make_dctx ?dict ?params () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make (Bytes.Writer.slice_length w) in
  let error = writer_error w in
  let eof = ref false (* true on end of frames *) in
  let rec decompress ~error ctx ~src ~dst =
    match decompress_stream ctx ~src ~dst with
    | exception Failure e -> (* cannot free ctx here *) error e
    | is_eof ->
        eof := is_eof;
        let flush_dst = Zbuf.dst_is_full dst && not is_eof in
        if not (Zbuf.dst_is_empty dst) then begin
          let slice = Zbuf.dst_to_slice dst in
          Zbuf.dst_clear dst; Bytes.Writer.write w slice;
        end;
        if not (Zbuf.src_is_consumed src) || flush_dst
        then decompress ~error ctx ~src ~dst else () (* await *)
  in
  let write = function
  | slice when Bytes.Slice.is_eod slice ->
      free_dctx ctx; (* Note: [write] is never called again *)
      if !eof then (if eod then Bytes.Writer.write_eod w) else
      error err_unexp_eod
  | slice ->
      Zbuf.src_set_slice src slice; decompress ~error ctx ~src ~dst
  in
  Bytes.Writer.make ?pos ~slice_length write

(* Compression *)

type cctx (* Custom value holding a ZSTD_CCtx. We manually deallocate
  them when we know but they have a finalizer which accepts NULL pointers. *)

external create_cctx : unit -> cctx = "ocaml_bytesrw_ZSTD_createCCtx"
external free_cctx : cctx -> unit = "ocaml_bytesrw_ZSTD_freeCCtx"

external cctx_set_parameter : cctx -> c_parameter -> int -> unit =
  "ocaml_bytesrw_ZSTD_CCtx_setParameter"

external cctx_load_dictionary : cctx -> string -> unit =
  "ocaml_bytesrw_ZSTD_CCtx_loadDictionary"

external compress_stream2 :
  cctx -> src:Zbuf.t -> dst:Zbuf.t -> end_dir:end_directive -> bool =
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

  let set ctx p = (* raises Failure *)
    let set_unsafe (n, v) = cctx_set_parameter ctx (C_unsafe n) v in
    cctx_set_parameter ctx C_checksumflag(if p.checksum then 1 else 0);
    cctx_set_parameter ctx C_compressionlevel p.clevel;
    cctx_set_parameter ctx C_windowlog p.window_log;
    List.iter set_unsafe p.unsafe_params

  let checksum p = p.checksum
  let clevel p = p.clevel
  let window_log p = p.window_log
  let unsafe_param n v p = { p with unsafe_params = (n, v) :: p.unsafe_params }
end

module Cdict = struct (* Could be expanded to full ZSTD_CDict support. *)
  type t = string
  let of_binary_string = Fun.id
end

let make_cctx ?dict ?(params = Cctx_params.default) () =
  match create_cctx () with
  | exception Failure e -> error e
  | ctx ->
      try
        Cctx_params.set ctx params;
        Option.iter (cctx_load_dictionary ctx) dict;
        ctx
      with Failure e -> free_cctx ctx; error e

type compress_state = Await | Flush | Flush_eod | Eod

let compress_reads
    ?dict ?params () ?pos ?(slice_length = cstream_out_size ()) r
  =
  let ctx = make_cctx ?dict ?params () in
  let src = Zbuf.make_empty () and dst = Zbuf.make slice_length in
  let error = reader_error r in
  let state = ref Await in
  (* The following invariant must hold. [free_cctx ctx] is only ever
     called after [state] becomes Eod. This state is sticky and any
     read in this state returns [Bytes.Slice.eod]. *)
  let rec compress_eod ~error ctx ~src ~dst =
    match compress_stream2 ctx ~src ~dst ~end_dir:E_end with
    | exception Failure e -> state := Eod; free_cctx ctx; error e
    | is_eos ->
        if not is_eos && Zbuf.dst_is_empty dst
        then compress_eod ~error ctx ~src ~dst else
        let slice = match Zbuf.dst_is_empty dst with
        | true -> Bytes.Slice.eod | false -> Zbuf.dst_to_slice dst
        in
        if is_eos
        then (state := Eod; free_cctx ctx; slice)
        else (Zbuf.dst_clear dst; slice)
  in
  let rec compress ~error ctx ~src ~dst =
    match compress_stream2 ctx ~src ~dst ~end_dir:E_continue with
    | exception Failure e -> state := Eod; free_cctx ctx; error e
    | _is_eos ->
        if Zbuf.dst_is_empty dst then (state := Await; read ()) else
        let slice = Zbuf.dst_to_slice dst in
        let flush = Zbuf.dst_is_full dst || not (Zbuf.src_is_consumed src)in
        state := if flush then Flush else Await;
        Zbuf.dst_clear dst;
        slice
  and read () = match !state with
  | Await ->
      begin match Bytes.Reader.read r with
      | slice when Bytes.Slice.is_eod slice ->
          state := Flush_eod; compress_eod ~error ctx ~src ~dst
      | slice ->
          Zbuf.src_set_slice src slice; compress ~error ctx ~src ~dst
      end
  | Flush -> compress ~error ctx ~src ~dst
  | Flush_eod -> compress_eod ~error ctx ~src ~dst
  | Eod -> Bytes.Slice.eod
  in
  Bytes.Reader.make ?pos ~slice_length read

let compress error ctx ~src ~dst ~end_dir =
  match compress_stream2 ctx ~src ~dst ~end_dir with
  | is_eodir -> is_eodir | exception Failure e -> free_cctx ctx; error e

let compress_writes
    ?dict ?params () ?pos ?(slice_length = cstream_in_size ()) ~eod w
  =
  let ctx = make_cctx ?dict ?params () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make (Bytes.Writer.slice_length w) in
  let error = writer_error w in
  let write_dst w dst =
    if Zbuf.dst_is_empty dst then () else
    (Bytes.Writer.write w (Zbuf.dst_to_slice dst); Zbuf.dst_clear dst)
  in
  let rec compress_eod ~error w ctx ~src ~dst = (* N.B. only gets call once *)
    match compress_stream2 ctx ~src ~dst ~end_dir:E_end with
    | exception Failure e -> free_cctx ctx; error e
    | is_eos ->
        write_dst w dst;
        if is_eos
        then (free_cctx ctx; if eod then Bytes.Writer.write_eod w)
        else compress_eod ~error w ctx ~src ~dst
  in
  let rec compress ~error w ctx ~src ~dst =
    match compress_stream2 ctx ~src ~dst ~end_dir:E_continue with
    | exception Failure e -> (* cannot free ctx here *) error e
    | _is_eos ->
        let flush = Zbuf.dst_is_full dst || not (Zbuf.src_is_consumed src) in
        write_dst w dst;
        if flush then compress ~error w ctx ~src ~dst else () (* await *)
  in
  let write = function
  | slice when Bytes.Slice.is_eod slice -> compress_eod ~error w ctx ~src ~dst
  | slice -> Zbuf.src_set_slice src slice; compress ~error w ctx ~src ~dst
  in
  Bytes.Writer.make ?pos ~slice_length write
