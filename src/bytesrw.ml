(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Bytes = struct
  include Bytes

  module Slice = struct
    let io_buffer_size = 65536
    let unix_io_buffer_size = 65536

    let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

    let err_invalid_slice ~first ~length ~len =
      invalid_argf "invalid slice first:%d length:%d bytes:%d" first length len

    let err_slice_length l =
      invalid_argf "slice length %d is not positive" l

    let check_slice_length l = if l <= 0 then err_slice_length l else l

    type t = { bytes : Bytes.t; first : int; length : int }
    let make bytes ~first ~length =
      let len = Bytes.length bytes in
      if length <= 0 || length > len || first < 0 || first >= len
      then err_invalid_slice ~first ~length ~len;
      { bytes; first; length }

    let bytes s = s.bytes
    let first s = s.first
    let length s = s.length
    let copy ~tight s =
      if not tight then { s with bytes = Bytes.copy s.bytes } else
      let bytes = Bytes.sub s.bytes s.first s.length in
      { bytes; first = 0; length = s.length}

    let eod = { bytes = Bytes.empty; first = 0; length = 0 }
    let is_eod s = s == eod

    let take n s =
      if n <= 0 then eod else
      if n >= s.length then s else
      { s with length = n }

    let drop n s =
      if n <= 0 then s else
      if n >= s.length then eod else
      { s with first = s.first + n }

    let break n s = take n s, drop n s

    let of_bytes ?(first = 0) ?last bytes =
      let max = Bytes.length bytes - 1 in
      let last = match last with
      | None -> max | Some last -> if last > max then last else last
      in
      let first = if first < 0 then 0 else first in
      if first > last then eod else
      { bytes; first; length = last - first + 1 }

    let to_bytes s = Bytes.sub s.bytes s.first s.length
    let to_string s = Bytes.sub_string s.bytes s.first s.length
    let add_to_buffer b s = Buffer.add_subbytes b s.bytes s.first s.length

    let pp_head_hex ell ppf head =
      let pp_hex ppf x =
        for i = 0 to String.length head - 1 do
          Format.fprintf ppf "%02x" (Char.code head.[i])
        done
      in
      Format.fprintf ppf "x%a%s" pp_hex head (if ell then "" else "…")

    let pp_head_text ell ppf head =
      Format.fprintf ppf "\"%s%s\"" head (if ell then "" else "…")

    let _pp pp_head ppf s =
      if is_eod s then Format.pp_print_string ppf "<eod>" else
      let hlen = Int.min s.length 4 in
      let head = Bytes.(unsafe_to_string (sub s.bytes s.first hlen)) in
      let ell = hlen = s.length in
      Format.fprintf ppf "@[<h>{range:[%04d;%04d] len:%04d %a}@]"
      s.first (s.first + s.length - 1) s.length (pp_head ell) head

    let pp ppf s = _pp pp_head_hex ppf s
    let pp_text ppf s = _pp pp_head_text ppf s
    let tracer ?(pp = pp) ?(ppf = Format.err_formatter)  ~id s =
      Format.fprintf ppf "@[<h>%s: %a@]@." id pp s
  end

  module Reader = struct
    type t =
      { stream_offset : int;
        read : unit -> Slice.t;
        slice_length : int option; }

    let make ?(stream_offset = 0) ?slice_length read =
      let slice_length = Option.map Slice.check_slice_length slice_length in
      { stream_offset; read; slice_length }

    let read_eod = Fun.const Slice.eod
    let empty = { stream_offset = 0; read = read_eod; slice_length = None }
    let stream_offset r = r.stream_offset
    let slice_length r = r.slice_length
    let read r = r.read ()

    let of_in_channel ?stream_offset ?(slice_length = Slice.io_buffer_size) ic =
      let () = In_channel.set_binary_mode ic true in
      let slice_length = Slice.check_slice_length slice_length in
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
      | None -> blen | Some slen -> Int.min (Slice.check_slice_length slen) blen
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
          let b = Bytes.unsafe_to_string (Slice.bytes s) in
          Out_channel.output_substring oc b (Slice.first s) (Slice.length s);
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
        slice_length : int option; }

    let make ?(stream_offset = 0) ?slice_length write =
      let slice_length = Option.map Slice.check_slice_length slice_length in
      { stream_offset; write; slice_length }

    let slice_length w = w.slice_length
    let stream_offset w = w.stream_offset
    let write w slice = w.write slice
    let write_eod w = write w Slice.eod
    let write_reader w r =
      let rec loop w r = match Reader.read r with
      | slice when Slice.is_eod slice -> ()
      | slice -> write w slice; loop w r
      in
      loop w r

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

    let write_in_channel w ic =
      let () = In_channel.set_binary_mode ic true in
      let rec loop w ic buf =
        let count = In_channel.input ic buf 0 (Bytes.length buf) in
        if count = 0 then () else
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
end
