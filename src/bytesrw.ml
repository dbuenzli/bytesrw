(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Bytes = struct
  include Bytes

  let invalid_argf fmt = Printf.ksprintf invalid_arg fmt
  let err_non_positive_length s =
    invalid_argf "slice length %d is not positive" s

  let err_invalid_slice ~first ~length ~len =
    invalid_argf "invalid slice first:%d length:%d bytes:%d" first length len

  module Slice = struct
    type t = { bytes : Bytes.t; first : int; length : int }
    let make bytes ~first ~length =
      let len = Bytes.length bytes in
      if length <= 0 || length > len || first < 0 || first >= len
      then err_invalid_slice ~first ~length ~len;
      { bytes; first; length }

    let bytes s = s.bytes
    let first s = s.first
    let length s = s.length
    let eod = { bytes = Bytes.empty; first = 0; length = 0 }
    let is_eod s = s == eod
    let to_string s = Bytes.sub_string s.bytes s.first s.length
  end

  module Reader = struct
    type t = unit -> Slice.t

    let make = Fun.id
    let read br = br ()

    let of_in_channel ?(buf = Bytes.create 65536) ic =
      let () = In_channel.set_binary_mode ic true in
      let bf () =
        let length = In_channel.input ic buf 0 (Bytes.length buf) in
        if length = 0 then Slice.eod else Slice.make buf ~first:0 ~length
      in
      make bf

    let of_bytes ?slice_length b =
      let len = Bytes.length b in
      if len = 0 then Fun.const Slice.eod else
      let length = match slice_length with
      | None -> len
      | Some s when s <= 0 -> err_non_positive_length s
      | Some s -> Int.min s len
      in
      match length = len with
      | true ->
          let s = ref (Slice.make b ~first:0 ~length) in
          let bf () = let v = !s in s := Slice.eod; v in
          make bf
      | false ->
          let first = ref 0 in
          let bf () =
            if !first >= len then Slice.eod else
            let length = Int.min length (len - !first) in
            let s = Slice.make b ~first:!first ~length in
            first := !first + length; s
          in
          make bf

    let of_string ?slice_length s =
      (* Unsafe is ok: the consumer is not supposed to mutate the bytes. *)
      of_bytes ?slice_length (Bytes.unsafe_of_string s)

    let rec to_buffer b r = match r () with
    | s when Slice.is_eod s -> ()
    | s ->
        Slice.(Buffer.add_subbytes b (bytes s) (first s) (length s));
        to_buffer b r

    let to_string r =
      let b = Buffer.create 1024 in
      to_buffer b r; Buffer.contents b
  end

  module Writer = struct
    type t = Slice.t -> unit

    let make = Fun.id

    let write w s = w s
    let write_bytes ?slice_length w b =
      let rec loop w b len first length =
        if first >= len then () else
        let length = Int.min length (len - first) in
        w (Slice.make b ~first ~length);
        loop w b len (first + length) length
      in
      let len = Bytes.length b in
      let length = match slice_length with
      | None -> len
      | Some s when s <= 0 -> err_non_positive_length s
      | Some s -> Int.min s len
      in
      loop w b len 0 length

    let write_string ?slice_length w s =
      (* Unsafe is ok: the writer is not supposed to mutate the bytes. *)
      write_bytes ?slice_length w (Bytes.unsafe_of_string s)

    let write_eod w = w Slice.eod

    let of_out_channel oc =
      let () = Out_channel.set_binary_mode oc true in
      function
      | s when Slice.is_eod s -> Out_channel.flush oc
      | s -> Slice.(Out_channel.output oc (bytes s) (first s) (length s))

    let of_buffer b = function
    | s when Slice.is_eod s -> ()
    | s -> Slice.(Buffer.add_subbytes b (bytes s) (first s) (length s))

  end
end
