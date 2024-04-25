(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Bytes = struct
  include Bytes

  let invalid_argf fmt = Printf.ksprintf invalid_arg fmt

  module Slice = struct
    let err_invalid ~first ~length ~len =
      invalid_argf "invalid slice: first:%d length:%d bytes:%d" first length len

    let err_invalid_sub ~first ~length ~len =
      invalid_argf "invalid subslice: first:%d length:%d slice:%d"
        first length len

    let err_empty_range ~first ~last ~len =
      invalid_argf "invalid slice: first:%d last:%d bytes:%d" first last len

    (* Slice lengths *)

    type length = int

    let err_length l = invalid_argf "invalid slice length: %d is not positive" l
    let check_length l = if l <= 0 then err_length l else l
    let pp_length_option ppf = function
    | None -> Format.pp_print_string ppf "<none>"
    | Some len -> Format.pp_print_int ppf len

    (* Slices *)

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

    let copy ~tight s =
      if s.length = 0 then eod else
      if not tight then { s with bytes = Bytes.copy s.bytes } else
      let bytes = Bytes.sub s.bytes s.first s.length in
      { bytes; first = 0; length = s.length}

    (* Properties *)

    let bytes s = s.bytes
    let first s = s.first
    let length s = s.length

    (* Breaking *)

    let take n s =
      if n <= 0 || is_eod s then None else
      if n >= s.length then Some s else
      Some { s with length = n }

    let drop n s =
      if n >= s.length || is_eod s then None else
      if n <= 0 then Some s else
      Some { s with first = s.first + n; length = s.length - n }

    let break n s = match take n s with
    | None -> None
    | Some l -> (match drop n s with None -> None | Some r -> Some (l, r))

    let sub' ~allow_eod s ~first ~length =
      let max = s.length - 1 in
      if first < 0 || first > max ||
         length < 0 || first + length > max ||
         (length = 0 && not allow_eod)
      then err_invalid_sub ~first ~length ~len:(max + 1) else
      if length = 0 then eod else
      { bytes = s.bytes; first = s.first + first; length }

    let sub s ~first ~length = sub' ~allow_eod:false s ~first ~length
    let sub_or_eod s ~first ~length = sub' ~allow_eod:true s ~first ~length

    let subrange' ~allow_eod ?(first = 0) ?last s =
      let max = length s - 1 in
      let last = match last with
      | None -> max | Some last -> if last > max then max else last
      in
      let first = if first < 0 then 0 else first in
      if first <= last then
        { bytes = s.bytes; first = s.first + first; length = last - first + 1 }
      else
      if allow_eod then eod else
      err_empty_range ~first ~last ~len:(max + 1)

    let subrange ?first ?last s =
      subrange' ~allow_eod:false ?first ?last s

    let subrange_or_eod ?first ?last s =
      subrange' ~allow_eod:true ?first ?last s

    (* Converting *)

    let of_bytes' ~allow_eod ?(first = 0) ?last bytes =
      let max = Bytes.length bytes - 1 in
      let last = match last with
      | None -> max | Some last -> if last > max then max else last
      in
      let first = if first < 0 then 0 else first in
      if first <= last then { bytes; first; length = last - first + 1 } else
      if allow_eod then eod else err_empty_range ~first ~last ~len:(max + 1)

    let of_bytes ?first ?last bytes =
      of_bytes' ~allow_eod:false ?first ?last bytes

    let of_bytes_or_eod ?first ?last bytes =
      of_bytes' ~allow_eod:true ?first ?last bytes

    let of_string s = of_bytes (Bytes.of_string s)
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

  module Stream = struct
    type pos = int
    let err_channel_pos kind pos =
      Printf.sprintf "%s: channel position %Ld doesn't fit on int" kind pos
  end

  module Reader = struct
    type t =
      { parent_pos : Stream.pos;
        mutable pos : int;
        mutable read : unit -> Slice.t;
        slice_length : Slice.length option }

    let make ?(parent_pos = 0) ?slice_length read =
      let slice_length = Option.map Slice.check_length slice_length in
      { parent_pos; pos = 0; read; slice_length }

    let make' ~parent ?slice_length read =
      let parent_pos = parent.pos in
      let slice_length = match slice_length with
      | None -> parent.slice_length | Some l -> Option.map Slice.check_length l
      in
      { parent_pos; pos = 0; read; slice_length }

    let read_eod = Fun.const Slice.eod
    let empty () =
      { parent_pos = 0; pos = 0; read = read_eod; slice_length = None }

    (* Stream properties *)

    let parent_pos r = r.parent_pos
    let pos r = r.pos
    let read_length = pos
    let slice_length r = r.slice_length
    let with_props ?from ?parent_pos ?pos ?slice_length r =
      let d = Option.value ~default:r from in
      let parent_pos = Option.value ~default:d.parent_pos parent_pos in
      let pos = Option.value ~default:d.pos pos in
      let slice_length = Option.value ~default:d.slice_length slice_length in
      { r with parent_pos; pos; slice_length; }

    (* Reading *)

    let read r =
      let slice = r.read () in
      let len = Slice.length slice in
      (if len = 0 then r.read <- read_eod);
      r.pos <- r.pos + len; slice

    (* Push back

       We disallow pushing back Slice.eod because we want to
       be able to accurately detect the end of stream condition. *)

    let err_eod_push = "cannot push back eod"

    let push_back r s =
      let read = r.read in
      if Slice.is_eod s then invalid_arg err_eod_push else
      let next_read () = r.read <- read; s in
      r.read <- next_read;
      r.pos <- r.pos - Slice.length s

    let err_negative_sniff n = invalid_argf "negative sniff length: %d" n
    let sniff n r =
      if n < 0 then err_negative_sniff n else
      if n = 0 then "" else
      let rec loop b i rem s = match read s with
      | s when Slice.is_eod s ->
          let sniff_len = n - rem  in
          if sniff_len = 0 then "" else
          let back = Slice.make b ~first:0 ~length:sniff_len in
          push_back r back;
          Bytes.sub_string b 0 sniff_len
      | s ->
          let slen = Slice.length s in
          let n = Int.min slen rem in
          Bytes.blit (Slice.bytes s) (Slice.first s) b i n;
          let rem = rem - n in
          if rem > 0 then loop b (i + n) rem r else
          let s0 = Slice.make b ~first:0 ~length:(Bytes.length b) in
          (match Slice.drop n s with None -> () | Some s1 -> push_back r s1);
          push_back r s0;
          (* Unsafe is ok: the consumer of s0 is not supposed to mutate the
             bytes. *)
          Bytes.unsafe_to_string b
      in
      loop (Bytes.create n) 0 n r

    (* Converting *)

    let of_bytes ?parent_pos ?slice_length b =
      let blen = Bytes.length b in
      if blen = 0 then make ?parent_pos ?slice_length read_eod else
      let slice_length = match slice_length with
      | None -> blen | Some slen -> Int.min (Slice.check_length slen) blen
      in
      if slice_length = blen then begin
        let s = ref (Slice.make b ~first:0 ~length:blen) in
        let read () = let v = !s in s := Slice.eod; v in
        make ?parent_pos ~slice_length read
      end else begin
        let first = ref 0 in
        let read () =
          if !first >= blen then Slice.eod else
          let length = Int.min slice_length (blen - !first) in
          let s = Slice.make b ~first:!first ~length in
          first := !first + length; s
        in
        make ?parent_pos ~slice_length read
      end

    let of_string ?parent_pos ?slice_length s =
      (* Unsafe is ok: the consumer is not supposed to mutate the bytes. *)
      of_bytes ?parent_pos ?slice_length (Bytes.unsafe_of_string s)

    let of_in_channel ?parent_pos ?(slice_length = Slice.io_buffer_size) ic =
      let () = In_channel.set_binary_mode ic true in
      let slice_length = Slice.check_length slice_length in
      let b = Bytes.create slice_length in
      let read () =
        let count = In_channel.input ic b 0 (Bytes.length b) in
        if count = 0 then Slice.eod else Slice.make b ~first:0 ~length:count
      in
      let parent_pos = match parent_pos with
      | Some p -> p
      | None ->
          let pos = In_channel.pos ic in
          match Int64.unsigned_to_int pos with
          | Some p -> p
          | None ->
              raise (Sys_error (Stream.err_channel_pos "Bytes.Reader" pos))
      in
      make ~parent_pos ~slice_length read

    let of_slice_seq ?parent_pos ?slice_length seq =
      let seq = ref seq in
      let read () = match !seq () with
      | Seq.Nil -> Slice.eod | Seq.Cons (slice, next) -> seq := next; slice
      in
      make ?parent_pos ?slice_length read

    let rec add_to_buffer b r = match read r with
    | s when Slice.is_eod s -> ()
    | s -> Slice.add_to_buffer b s; add_to_buffer b r

    let to_string r =
      let blen = Option.value ~default:1024 r.slice_length in
      let b = Buffer.create blen in
      add_to_buffer b r; Buffer.contents b

    let to_slice_seq r =
      let dispense () = match read r with
      | slice when Slice.is_eod slice -> None
      | slice -> Some slice
      in
      Seq.of_dispenser dispense

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

    (* Formatting *)

    let pp ppf r =
      Format.fprintf ppf "@[<1><reader pos:%d parent_pos:%d slice:%a>@]"
        r.pos r.parent_pos Slice.pp_length_option r.slice_length

    let trace_reads f r =
      let read () = let slice = r.read () in f slice; slice in
      { r with read }
  end

  module Writer = struct
    type t =
      { parent_pos : int;
        mutable pos : int;
        slice_length : Slice.length option;
        mutable write : Slice.t -> unit; }

    let make ?(parent_pos = 0) ?slice_length write =
      let slice_length = Option.map Slice.check_length slice_length in
      { parent_pos; pos = 0; slice_length; write }

    let make' ~parent ?slice_length write =
      let parent_pos = parent.pos in
      let slice_length = match slice_length with
      | None -> parent.slice_length | Some l -> Option.map Slice.check_length l
      in
      { parent_pos; pos = 0; slice_length; write }

    let ignore_write s = ()
    let ignore () =
      { parent_pos = 0; pos = 0; slice_length = None; write = ignore_write }

    (* Stream properties *)

    let parent_pos w = w.parent_pos
    let pos w = w.pos
    let slice_length w = w.slice_length
    let written_length w = w.pos

    let with_props ?from ?parent_pos ?pos ?slice_length w =
      let d = Option.value ~default:w from in
      let parent_pos = Option.value ~default:d.parent_pos parent_pos in
      let pos = Option.value ~default:d.pos pos in
      let slice_length = Option.value ~default:d.slice_length slice_length in
      { w with parent_pos; pos; slice_length }

    (* Writing *)

    let write_only_eod s =
      if Slice.is_eod s then () else invalid_arg "slice written after eod"

    let write w slice =
      let write = w.write in
      let len = Slice.length slice in
      (if len = 0 then w.write <- write_only_eod);
      w.pos <- w.pos + len;
      write slice

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

    (* Converting *)

    let of_out_channel
        ?parent_pos ?(slice_length = Slice.io_buffer_size)
        ?(flush_slices = false) oc
      =
      let () = Out_channel.set_binary_mode oc true in
      let write = function
      | s when Slice.is_eod s -> ()
      | s ->
          Slice.(Out_channel.output oc (bytes s) (first s) (length s));
          if flush_slices then Out_channel.flush oc
      in
      let parent_pos = match parent_pos with
      | Some pos -> pos
      | None ->
          let pos = Out_channel.pos oc in
          match Int64.unsigned_to_int pos with
          | Some pos -> pos
          | None ->
              raise (Sys_error (Stream.err_channel_pos "Bytes.Writer" pos))
      in
      make ~parent_pos ~slice_length write

    let of_buffer ?parent_pos ?slice_length b =
      let write = function
      | s when Slice.is_eod s -> ()
      | s -> Slice.(Buffer.add_subbytes b (bytes s) (first s) (length s))
      in
      make ?parent_pos ?slice_length write

    (* Formatting *)

    let pp ppf w =
      Format.fprintf ppf "@[<1><writer pos:%d parent_pos:%d slice:%a>@]"
        w.pos w.parent_pos Slice.pp_length_option w.slice_length

    let trace_writes f w =
      let write slice = f slice; w.write slice in
      { w with write }
  end

  let pp_hex = Bytesrw_fmt.pp_hex
end
