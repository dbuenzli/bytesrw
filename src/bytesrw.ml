(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Bytes = struct
  include Bytes

  module Slice = struct
    let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

    let err_invalid ~first ~length ~len =
      invalid_argf "invalid slice: first:%d length:%d bytes:%d" first length len

    let err_empty_range ~first ~last ~len =
      invalid_argf "invalid slice: first:%d last:%d bytes:%d" first last len

    let err_length l =
      invalid_argf "invalid slice length: %d is not positive" l

    let check_length l = if l <= 0 then err_length l else l

    type t = { bytes : Bytes.t; first : int; length : int }

    let eod = { bytes = Bytes.empty; first = 0; length = 0 }
    let is_eod s = s == eod

    let make bytes ~first ~length =
      let len = Bytes.length bytes in
      if length <= 0 || length > len || first < 0 || first >= len
      then err_invalid ~first ~length ~len;
      { bytes; first; length }

    let make_or_eod bytes ~first ~length =
      let len = Bytes.length bytes in
      if length > len || first < 0 || first >= len
      then err_invalid ~first ~length ~len;
      if length = 0 then eod else { bytes; first; length }

    let bytes s = s.bytes
    let first s = s.first
    let length s = s.length
    let copy ~tight s =
      if s.length = 0 then eod else
      if not tight then { s with bytes = Bytes.copy s.bytes } else
      let bytes = Bytes.sub s.bytes s.first s.length in
      { bytes; first = 0; length = s.length}

    (* Breaking *)

    let take n s =
      if n <= 0 then eod else
      if n >= s.length then s else
      { s with length = n }

    let drop n s =
      if n <= 0 then s else
      if n >= s.length then eod else
      { s with first = s.first + n }

    let break n s = take n s, drop n s

    (* Converting *)

    let of_bytes' ~allow_eod ?(first = 0) ?last bytes =
      let max = Bytes.length bytes - 1 in
      let last = match last with
      | None -> max | Some last -> if last > max then max else last
      in
      let first = if first < 0 then 0 else first in
      if first <= last then { bytes; first; length = last - first + 1 } else
      if allow_eod then eod else
      err_empty_range ~first ~last ~len:(max + 1)

    let of_bytes ?first ?last bytes =
      of_bytes' ~allow_eod:false ?first ?last bytes

    let of_bytes_or_eod ?first ?last bytes =
      of_bytes' ~allow_eod:true ?first ?last bytes

    let to_bytes s = Bytes.sub s.bytes s.first s.length
    let to_string s = Bytes.sub_string s.bytes s.first s.length
    let add_to_buffer b s = Buffer.add_subbytes b s.bytes s.first s.length
    let output_to_out_channel oc s =
      let b = Bytes.unsafe_to_string s.bytes in
      Out_channel.output_substring oc b s.first s.length

    (* Formatting *)

    let pp_meta ppf s =
      Format.fprintf ppf "@[[%04d;%04d]@ len:%04d@]"
        s.first (s.first + s.length - 1) s.length

    let pp_full ~hex ppf s =
      let pp_bytes ppf s=
        if not hex
        then Bytesrw_fmt.pp_raw ~first:s.first ~len:s.length ppf s.bytes
        else Bytesrw_fmt.pp_hex ~addr:true ~ascii:true ~start:s.first
            ~len:s.length () ppf s.bytes
      in
      Format.fprintf ppf "@[<v>%a@,%a@]" pp_meta s pp_bytes s

    let pp_head ~hex c ppf s =
      let pp_head = Bytesrw_fmt.(if hex then pp_head_hex else pp_head_raw) c in
      Format.fprintf ppf "@[%a %a@]" pp_meta s pp_head s.bytes

    let pp' ?(head = 4) ?(hex = true) () ppf s =
      if is_eod s then Format.pp_print_string ppf "<eod>" else
      if head = -1 then pp_full ~hex ppf s else pp_head ~hex head ppf s

    let pp = pp' ()

    let tracer ?(pp = pp) ?(ppf = Format.err_formatter)  ~id s =
      Format.fprintf ppf "@[[%3s]: @[%a@]@]@." id pp s

    (* Other *)

    let io_buffer_size = 65536
    let unix_io_buffer_size = 65536
  end

  module Reader = struct
    type t =
      { stream_offset : int;
        read : unit -> Slice.t;
        slice_length : int option;
        mutable read_length : int; }

    let make ?(stream_offset = 0) ?slice_length read =
      let slice_length = Option.map Slice.check_length slice_length in
      { stream_offset; read; slice_length; read_length = 0 }

    let of_reader ?stream_offset ?slice_length r read =
      let stream_offset = Option.value ~default:r.stream_offset stream_offset in
      let slice_length = match slice_length with
      | None -> r.slice_length | Some l -> Some (Slice.check_length l)
      in
      let read_length = r.read_length in
      { stream_offset; read; slice_length; read_length }

    let read_eod = Fun.const Slice.eod
    let empty =
      { stream_offset = 0; read = read_eod; slice_length = None;
        read_length = 0 }

    let stream_offset r = r.stream_offset
    let slice_length r = r.slice_length
    let read_length r = r.read_length
    let read r =
      let slice = r.read () in
      r.read_length <- r.read_length + Slice.length slice; slice

    let of_in_channel ?stream_offset ?(slice_length = Slice.io_buffer_size) ic =
      let () = In_channel.set_binary_mode ic true in
      let slice_length = Slice.check_length slice_length in
      let b = Bytes.create slice_length in
      let read () =
        let count = In_channel.input ic b 0 (Bytes.length b) in
        if count = 0 then Slice.eod else Slice.make b ~first:0 ~length:count
      in
      make ?stream_offset ~slice_length read

    let of_bytes ?stream_offset ?slice_length b =
      let blen = Bytes.length b in
      if blen = 0 then make ?stream_offset ?slice_length read_eod else
      let slice_length = match slice_length with
      | None -> blen | Some slen -> Int.min (Slice.check_length slen) blen
      in
      if slice_length = blen then begin
        let s = ref (Slice.make b ~first:0 ~length:blen) in
        let read () = let v = !s in s := Slice.eod; v in
        make ?stream_offset ~slice_length read
      end else begin
        let first = ref 0 in
        let read () =
          if !first >= blen then Slice.eod else
          let length = Int.min slice_length (blen - !first) in
          let s = Slice.make b ~first:!first ~length in
          first := !first + length; s
        in
        make ?stream_offset ~slice_length read
      end

    let of_string ?stream_offset ?slice_length s =
      (* Unsafe is ok: the consumer is not supposed to mutate the bytes. *)
      of_bytes ?stream_offset ?slice_length (Bytes.unsafe_of_string s)

    let rec add_to_buffer b r = match read r with
    | s when Slice.is_eod s -> ()
    | s -> Slice.add_to_buffer b s; add_to_buffer b r

    let to_string r =
      let blen = Option.value ~default:1024 r.slice_length in
      let b = Buffer.create blen in
      add_to_buffer b r; Buffer.contents b

    let output_to_out_channel ?(flush_slices = false) oc r =
      let () = Out_channel.set_binary_mode oc true in
      let rec loop r oc = match read r with
      | s when Slice.is_eod s -> ()
      | s ->
          Slice.output_to_out_channel oc s;
          if flush_slices then Out_channel.flush oc;
          loop r oc
      in
      loop r oc

    let trace_reads f r =
      let read () = let slice = r.read () in f slice; slice in
      { r with read }

    let get_stream_offset ~none = function
    | None -> none.stream_offset | Some i -> i
  end

  module Writer = struct
    type t =
      { stream_offset : int;
        write : Slice.t -> unit;
        slice_length : int option;
        mutable written_length : int }

    let make ?(stream_offset = 0) ?slice_length write =
      let slice_length = Option.map Slice.check_length slice_length in
      { stream_offset; write; slice_length; written_length = 0 }

    let of_writer ?stream_offset ?slice_length w write =
      let stream_offset = Option.value ~default:w.stream_offset stream_offset in
      let slice_length = match slice_length with
      | None -> w.slice_length | Some l -> Some (Slice.check_length l)
      in
      let written_length = w.written_length in
      { stream_offset; write; slice_length; written_length }

    let slice_length w = w.slice_length
    let stream_offset w = w.stream_offset
    let written_length w = w.written_length
    let write w slice =
      w.written_length <- w.written_length + Slice.length slice;
      w.write slice

    let write_eod w = write w Slice.eod

    let write_bytes w b =
      let rec loop w slice_length b blen first =
        if first >= blen then () else
        let length = Int.min slice_length (blen - first) in
        write w (Slice.make b ~first ~length);
        loop w slice_length b blen (first + length)
      in
      let blen = Bytes.length b in
      let slice_length = match w.slice_length with
      | None -> blen | Some slen -> Int.min slen blen
      in
      loop w slice_length b blen 0

    let write_string w s =
      (* Unsafe is ok: the writer is not supposed to mutate the bytes. *)
      write_bytes w (Bytes.unsafe_of_string s)

    let write_reader ~eod w r =
      let rec loop w r = match Reader.read r with
      | slice when Slice.is_eod slice -> if eod then write_eod w else ()
      | slice -> write w slice; loop w r
      in
      loop w r

    let write_in_channel ~eod w ic =
      let () = In_channel.set_binary_mode ic true in
      let rec loop w ic buf =
        let count = In_channel.input ic buf 0 (Bytes.length buf) in
        if count = 0 then (if eod then write_eod w else ())  else
        (write w (Slice.make buf ~first:0 ~length:count); loop w ic buf)
      in
      let blen = Option.value ~default:Slice.io_buffer_size w.slice_length in
      loop w ic (Bytes.create blen)

    let of_out_channel ?stream_offset ?slice_length ?(flush_slices = false) oc =
      let () = Out_channel.set_binary_mode oc true in
      let write = function
      | s when Slice.is_eod s -> ()
      | s ->
          Slice.(Out_channel.output oc (bytes s) (first s) (length s));
          if flush_slices then Out_channel.flush oc
      in
      make ?stream_offset ?slice_length write

    let of_buffer ?stream_offset ?slice_length b =
      let write = function
      | s when Slice.is_eod s -> ()
      | s -> Slice.(Buffer.add_subbytes b (bytes s) (first s) (length s))
      in
      make ?stream_offset ?slice_length write

    let get_stream_offset ~none = function
    | None -> none.stream_offset | Some i -> i

    let trace_writes f w =
      let write slice = f slice; w.write slice in
      { w with write }
  end

  let pp_hex = Bytesrw_fmt.pp_hex
end
