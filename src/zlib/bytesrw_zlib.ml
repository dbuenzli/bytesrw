(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

(* zlib compression levels. *)

type level = int
let default_compression = -1
let no_compression = 0
let best_speed = 1
let best_compression = 9

(* zlib flush values *)

type flush = (* keep order in sync with ocaml_zlib_flush table in C stubs *)
| Z_NO_FLUSH | Z_PARTIAL_FLUSH | Z_SYNC_FLUSH | Z_FULL_FLUSH
| Z_FINISH | Z_BLOCK | Z_TREES

(* zlib window bits values, there are no C constants for these  *)

let window_bits_deflate = -15
let window_bits_zlib = 15
let window_bits_gzip = 31

(* Errors. Stubs raise [Failure] in case of error which we then
   turn into Bytes.Stream.Error with the following error. *)

type Bytes.Stream.error += Error of string

let format_error ~format =
  let case msg = Error msg in
  let message = function Error msg -> msg | _ -> assert false in
  Bytes.Stream.make_format_error ~format ~case ~message

(* Library parameters *)

external version : unit -> string = "ocaml_bytesrw_zlib_version"

let default_slice_length = 131072 (* 128KB, our choice not zlib's one *)

(* [Zbuf.t] values are used to communicate buffers with C. For better
   or worse we use the same data structure that zstd uses. The
   conversion to libz's model occurs in the bindings on the C side *)

module Zbuf = struct
  type t = (* keep in sync with ocaml_zbuf_fields enum in C *)
    { mutable bytes : Bytes.t;
      mutable size : int; (* last read or write position + 1 *)
      mutable pos : int; (* next read or write position *) }

  let make_empty () = { bytes = Bytes.empty; size = 0; pos = 0 }
  let make ?slice_length () =
    let size = match slice_length with
    | None -> default_slice_length
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

  let src_rem buf = buf.size - buf.pos
  let src_to_slice buf =
    let src_rem = src_rem buf in
    if src_rem = 0 then Bytes.Slice.eod else
    Bytes.Slice.make buf.bytes ~first:buf.pos ~length:src_rem

  let dst_clear buf = buf.pos <- 0
  let dst_is_empty buf = buf.pos = 0
  let dst_is_full buf = buf.pos = buf.size
  let dst_to_slice buf = Bytes.Slice.make buf.bytes ~first:0 ~length:buf.pos
end

(* Decompression *)

type z_stream_inflate (* Custom value holding a z_stream. We manually
  deallocate them when we know but they have a finalizer which if not NULL
  yet calls inflateEnd and frees the pointer *)

external create_inflate_z_stream : window_bits:int -> z_stream_inflate =
  "ocaml_bytesrw_create_inflate_z_stream"

external free_inflate_z_stream : z_stream_inflate -> unit =
  "ocaml_bytesrw_free_inflate_z_stream"

external inflate : z_stream_inflate -> src:Zbuf.t -> dst:Zbuf.t -> bool =
  "ocaml_bytesrw_inflate"

external inflate_reset : z_stream_inflate -> unit =
  "ocaml_bytesrw_inflate_reset"

let make_z_stream_inflate ~error ~window_bits =
  match create_inflate_z_stream ~window_bits with
  | exception Failure e -> error e | zs -> zs

type decompress_eos_action = Parse_eod | Append_next | Stop
type decompress_state =
| Await | Eod | Eos of { leftover : Bytes.Slice.t } | Flush

let err_unexp_eod ?pos error = error ?pos "Unexpected end of compressed data"
let err_exp_eod ~leftover error =
  error ?pos:(Some (-(Bytes.Slice.length leftover))) "Expected end of data"

let inflate_reads ~error ~read_error ~eos_action ~window_bits ?slice_length r =
  let zs = make_z_stream_inflate ~error ~window_bits in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make ?slice_length () in
  let state = ref Await in
  (* The following invariant must hold. [free_inflate_z_stream] is
     called only if [!state = Eod]. This state can no longer mutate and
     any [read] in this state simply returns [Bytes.Slice.eod]. *)
  let rec decompress r zs ~src ~dst = match inflate zs ~src ~dst with
  | exception Failure e ->
      state := Eod; free_inflate_z_stream zs; read_error r ?pos:None e
  | eos ->
      state := begin
        if eos then Eos { leftover = Zbuf.src_to_slice src } else
        if not (Zbuf.src_is_consumed src) || (Zbuf.dst_is_full dst) then Flush
        else Await
      end;
      if Zbuf.dst_is_empty dst
      then read ()
      else let slice = Zbuf.dst_to_slice dst in (Zbuf.dst_clear dst; slice)
  and await r =
    let slice = Bytes.Reader.read r in
    if Bytes.Slice.is_eod slice
    then (state := Eod; free_inflate_z_stream zs; err_unexp_eod (read_error r))
    else (Zbuf.src_set_slice src slice; decompress r zs ~src ~dst)
  and append_next ~leftover r zs ~src ~dst =
    let slice = match Bytes.Slice.is_eod leftover with
    | true -> Bytes.Reader.read r | false -> leftover
    in
    if Bytes.Slice.is_eod slice
    then (state := Eod; free_inflate_z_stream zs; Bytes.Slice.eod) else
    match inflate_reset zs with
    | () -> Zbuf.src_set_slice src slice; decompress r zs ~src ~dst
    | exception Failure e ->
        state := Eod; free_inflate_z_stream zs; read_error r ?pos:None e
  and parse_eod ~leftover r zs =
    state := Eod; free_inflate_z_stream zs;
    if not (Bytes.Slice.is_eod leftover)
    then err_exp_eod ~leftover (read_error r) else
    let s = Bytes.Reader.read r in
    if Bytes.Slice.is_eod s then s else err_exp_eod ~leftover:s (read_error r)
  and stop ~leftover r zs =
    state := Eod; free_inflate_z_stream zs;
    Bytes.Reader.push_back r leftover;
    Bytes.Slice.eod
  and read () = match !state with
  | Flush -> decompress r zs ~src ~dst
  | Await -> await r
  | Eos { leftover } ->
      begin match eos_action with
      | Append_next -> append_next ~leftover r zs ~src ~dst
      | Parse_eod -> parse_eod ~leftover r zs
      | Stop -> stop ~leftover r zs
      end
  | Eod -> Bytes.Slice.eod
  in
  let slice_length = Some dst.Zbuf.size in
  Bytes.Reader.make' ~parent:r ~slice_length read

let inflate_writes ~error ~write_error ~window_bits ?slice_length w =
  let is_gzip = window_bits = window_bits_gzip in
  let zs = make_z_stream_inflate ~error ~window_bits in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make ?slice_length () in
  let error = write_error w in
  let eos = ref false in
  let rec decompress ~error zs ~src ~dst =
    match inflate zs ~src ~dst with
    | exception Failure e -> error ?pos:None e
    | is_eos ->
        if is_eos then begin
          eos := is_eos;
          if is_gzip then
            (match inflate_reset zs with
            | () -> () | exception Failure e -> write_error w ?pos:None e)
          else
          if not (Zbuf.src_is_consumed src)
          then err_exp_eod ~leftover:(Zbuf.src_to_slice src) (write_error w)
        end;
        let flush_dst = Zbuf.dst_is_full dst && not is_eos in
        if not (Zbuf.dst_is_empty dst) then begin
          let slice = Zbuf.dst_to_slice dst in
          Zbuf.dst_clear dst; Bytes.Writer.write w slice;
        end;
        if not (Zbuf.src_is_consumed src) || flush_dst
        then decompress ~error zs ~src ~dst else () (* await *)
  in
  let write = function
  | slice when Bytes.Slice.is_eod slice ->
      free_inflate_z_stream zs; (* Note: [write] is never called again *)
      if !eos then () else err_unexp_eod (write_error w)
  | slice ->
      Zbuf.src_set_slice src slice; decompress ~error zs ~src ~dst
  in
  let slice_length = match slice_length with
  | None -> Some default_slice_length | Some _ as l -> l
  in
  Bytes.Writer.make' ~parent:w ~slice_length write

(* Compression *)

type z_stream_deflate (* Custom value holding a z_stream. We manually
  deallocate them when we know but they have a finalizer which if not NULL
  yet calls deflateEnd and frees the pointer *)

external create_deflate_z_stream :
  level:int -> window_bits:int -> z_stream_deflate =
  "ocaml_bytesrw_create_deflate_z_stream"

external free_deflate_z_stream : z_stream_deflate -> unit =
  "ocaml_bytesrw_free_deflate_z_stream"

external deflate :
  z_stream_deflate -> src:Zbuf.t -> dst:Zbuf.t -> flush:flush -> bool =
  "ocaml_bytesrw_deflate"

external deflate_reset : z_stream_deflate -> unit =
  "ocaml_bytesrw_deflate_reset"

type compress_state = Await | Flush | Flush_eod | Eod

let make_z_stream_deflate ~error ~window_bits ?(level = default_compression) ()
  =
  match create_deflate_z_stream ~level ~window_bits with
  | exception Failure e -> error e | zs -> zs

let deflate_reads ~error ~read_error ~window_bits ?level ?slice_length r =
  let zs = make_z_stream_deflate ~error ~window_bits ?level () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make ?slice_length () in
  let state = ref Await in
  let error = read_error r in
  let rec compress_eod ~error zs ~src ~dst =
    match deflate zs ~src ~dst ~flush:Z_FINISH with
    | exception Failure e -> error ?pos:None e
    | is_eos ->
        if is_eos then (state := Eod; free_deflate_z_stream zs);
        if Zbuf.dst_is_empty dst then begin
          if not is_eos
          then compress_eod ~error zs ~src ~dst
          else Bytes.Slice.eod
        end else
        let slice = Zbuf.dst_to_slice dst in
        Zbuf.dst_clear dst;
        slice
  and compress ~error zs ~src ~dst =
    match deflate zs ~src ~dst ~flush:Z_NO_FLUSH with
    | exception Failure e -> error ?pos:None e
    | _is_eos ->
        if Zbuf.dst_is_empty dst then (state := Await; read ()) else
        let slice = Zbuf.dst_to_slice dst in
        let flush = Zbuf.dst_is_full dst || not (Zbuf.src_is_consumed src) in
        state := if flush then Flush else Await;
        Zbuf.dst_clear dst;
        slice
  and read () = match !state with
  | Flush -> compress ~error zs ~src ~dst
  | Flush_eod -> compress_eod ~error zs ~src ~dst
  | Await ->
      begin match Bytes.Reader.read r with
      | slice when Bytes.Slice.is_eod slice ->
          state := Flush_eod; compress_eod ~error zs ~src ~dst
      | slice ->
          Zbuf.src_set_slice src slice; compress ~error zs ~src ~dst
      end
  | Eod -> Bytes.Slice.eod
  in
  let slice_length = Some default_slice_length in
  Bytes.Reader.make' ~parent:r ~slice_length read

let deflate_writes ~error ~write_error ~window_bits ?level ?slice_length w =
  let zs = make_z_stream_deflate ~error ~window_bits ?level () in
  let src = Zbuf.make_empty () in
  let dst = Zbuf.make ?slice_length () in
  let error = write_error w in
  let write_dst w dst = match Zbuf.dst_is_empty dst with
  | true -> ()
  | false -> Bytes.Writer.write w (Zbuf.dst_to_slice dst); Zbuf.dst_clear dst
  in
  let rec compress_eod ~error w zs ~src ~dst = (* N.B. only gets called once. *)
    match deflate zs ~src ~dst ~flush:Z_FINISH with
    | exception Failure e -> error ?pos:None e
    | is_eos ->
        write_dst w dst;
        if is_eos
        then free_deflate_z_stream zs
        else compress_eod ~error w zs ~src ~dst
  in
  let rec compress ~error w zs ~src ~dst =
    match deflate zs ~src ~dst ~flush:Z_NO_FLUSH with
    | exception Failure e -> error ?pos:None e
    | _is_eos ->
        let flush = Zbuf.dst_is_full dst || not (Zbuf.src_is_consumed src) in
        write_dst w dst;
        if flush then compress ~error w zs ~src ~dst else () (* await *)
  in
  let write = function
  | slice when Bytes.Slice.is_eod slice -> compress_eod ~error w zs ~src ~dst
  | slice -> Zbuf.src_set_slice src slice; compress ~error w zs ~src ~dst
  in
  let slice_length = match slice_length with
  | None -> Some default_slice_length | Some _ as l -> l
  in
  Bytes.Writer.make' ~parent:w ~slice_length write

(* Compression formats *)

module Deflate = struct
  let window_bits = window_bits_deflate
  let format_error = format_error ~format:"deflate"
  let error = Bytes.Stream.error format_error
  let read_error = Bytes.Reader.read_error format_error
  let write_error = Bytes.Writer.write_error format_error

  let decompress_reads ?(leftover = false) ?slice_length r =
    let eos_action = if leftover then Stop else Parse_eod in
    inflate_reads ~error ~read_error ~eos_action ~window_bits ?slice_length r

  let decompress_writes ?slice_length w =
    inflate_writes ~error ~write_error ~window_bits ?slice_length w

  let compress_reads ?level ?slice_length r =
    deflate_reads ~error ~read_error ~window_bits ?level ?slice_length r

  let compress_writes ?level ?slice_length w =
    deflate_writes ~error ~write_error ~window_bits ?level ?slice_length w
end

module Zlib = struct
  let window_bits = window_bits_zlib
  let format_error = format_error ~format:"zlib"
  let error = Bytes.Stream.error format_error
  let read_error = Bytes.Reader.read_error format_error
  let write_error = Bytes.Writer.write_error format_error

  let decompress_reads ?(leftover = false) ?slice_length r =
    let eos_action = if leftover then Stop else Parse_eod in
    inflate_reads ~error ~read_error ~eos_action ~window_bits ?slice_length r

  let decompress_writes ?slice_length w =
    inflate_writes ~error ~write_error ~window_bits ?slice_length w

  let compress_reads ?level ?slice_length r =
    deflate_reads ~error ~read_error ~window_bits ?level ?slice_length r

  let compress_writes ?level ?slice_length w =
    deflate_writes ~error ~write_error ~window_bits ?level ?slice_length w
end

module Gzip = struct
  let window_bits = window_bits_gzip
  let format_error = format_error ~format:"gzip"
  let error = Bytes.Stream.error format_error
  let read_error = Bytes.Reader.read_error format_error
  let write_error = Bytes.Writer.write_error format_error

  let decompress_reads ?(all_members = true) ?slice_length r =
    let eos_action = if all_members then Append_next else Stop in
    inflate_reads ~error ~read_error ~eos_action ~window_bits ?slice_length r

  let decompress_writes ?slice_length w =
    inflate_writes ~error ~write_error ~window_bits ?slice_length w

  let compress_reads ?level ?slice_length r =
    deflate_reads ~error ~read_error ~window_bits ?level ?slice_length r

  let compress_writes ?level ?slice_length w =
    deflate_writes ~error ~write_error ~window_bits ?level ?slice_length w
end
