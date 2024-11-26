(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* UTF-8 decoder with position tracking using Bytes.Reader.t and the
   Stdlib codecs.

   FIXME: integrate the optimizations there were done in Jsont_bytesrw. *)

open Bytesrw

(* XXX add these things to Stdlib.Uchar *)

let uchar_max_utf_8_byte_length = 4
let uchar_utf_8_byte_decode_length byte = (* or utf_8_byte_length_of_byte *)
  if byte < 0x80 then 1 else if byte < 0xC2 then 0 else
  if byte < 0xE0 then 2 else if byte < 0xF0 then 3 else
  if byte < 0xF5 then 4 else 0

(* Decoder *)

let sot = 0x1A0000  (* start of text U+10FFFF + 1 *)
let eot = 0x1A0001  (*   end of text U+10FFFF + 2 *)

type decoder =
  { file : string;
    reader : Bytes.Reader.t;
    mutable i : Bytes.t; (* Current input slice. *)
    mutable i_max : int; (* Maximum byte index in [i]. *)
    mutable i_next : int; (* Next byte index to read in [i]. *)
    mutable overlap : Bytes.t; (* Buffer for overlapping decodes. *)
    mutable u : int; (* Current Unicode scalar value or sot or eot. *)
    mutable byte_count : int; (* Global byte count. *)
    mutable line : int; (* Current line number. *)
    mutable line_start : int; (* Current line first global byte position. *) }

let make_decoder ?(file = "-") reader =
  let overlap = Bytes.create uchar_max_utf_8_byte_length in
  { file; reader; i = overlap (* overwritten by initial refill *);
    i_max = 0; i_next = 1 (* triggers an initial refill *);
    overlap; u = sot; byte_count = 0; line = 1; line_start = 0; }

(* Decoder position and errors *)

let col_next d = d.byte_count - d.line_start
let err_loc d line col fmt =
  Format.kasprintf failwith ("%s:%d:%d: " ^^ fmt) d.file line col

let err_malformed_utf_8 d =
  err_loc d d.line (col_next d)
    "UTF-8 decoding error at input byte %d" d.byte_count

(* Next character *)

let[@inline] is_eoslice d = d.i_next > d.i_max
let[@inline] is_eod d = d.i_max = - 1 (* Only happens on Slice.eod *)
let[@inline] available d = d.i_max - d.i_next + 1
let[@inline] next_utf_8_length d =
  uchar_utf_8_byte_decode_length (Bytes.get_uint8 d.i d.i_next)

let set_slice d slice =
  d.i <- Bytes.Slice.bytes slice;
  d.i_next <- Bytes.Slice.first slice;
  d.i_max <- d.i_next + Bytes.Slice.length slice - 1

let rec setup_overlap d start need = match need with
| 0 ->
    let slice = match available d with
    | 0 -> Bytes.Reader.read d.reader
    | length -> Bytes.Slice.make d.i ~first:d.i_next ~length
    in
    d.i <- d.overlap; d.i_next <- 0; d.i_max <- start; slice
| need ->
    if is_eoslice d then set_slice d (Bytes.Reader.read d.reader);
    if is_eod d
    then (d.byte_count <- d.byte_count - start; err_malformed_utf_8 d);
    let available = available d in
    let take = Int.min need available in
    for i = 0 to take - 1 do
      Bytes.set d.overlap (start + i) (Bytes.get d.i (d.i_next + i))
    done;
    d.i_next <- d.i_next + take; d.byte_count <- d.byte_count + take;
    setup_overlap d (start + take) (need - take)

let rec nextc d = match available d with
| a when a <= 0 ->
    if is_eod d then d.u <- eot else
    (set_slice d (Bytes.Reader.read d.reader); nextc d)
| a when a < uchar_max_utf_8_byte_length && a < next_utf_8_length d ->
    let s = setup_overlap d 0 (next_utf_8_length d) in nextc d; set_slice d s
| _ ->
    let udec = Bytes.get_utf_8_uchar d.i d.i_next in
    if not (Uchar.utf_decode_is_valid udec) then err_malformed_utf_8 d else
    let u = Uchar.to_int (Uchar.utf_decode_uchar udec) in
    let ulen = Uchar.utf_decode_length udec in
    d.i_next <- d.i_next + ulen; d.byte_count <- d.byte_count + ulen;
    begin match u with
    | 0x000D (* CR *) -> d.line_start <- d.byte_count; d.line <- d.line + 1;
    | 0x000A (* LF *) ->
        d.line_start <- d.byte_count;
        if d.u <> 0x000D then d.line <- d.line + 1;
    | _ -> ()
    end;
    d.u <- u

(* UTF-8 encoder. *)

type encoder =
  { writer : Bytes.Writer.t; (* Destination of bytes. *)
    o : Stdlib.Bytes.t; (* Buffer for slices. *)
    o_max : int; (* Max index in [o]. *)
    mutable o_next : int; (* Next writable index in [o]. *) }

let[@inline] rem_len e = e.o_max - e.o_next + 1

let make_encoder ?buf:(o = Bytes.create 65535) writer =
  let len = Bytes.length o in
  if len < 4 then invalid_arg "encoder bytes buffer length < 4" else
  { writer; o; o_max = len - 1; o_next = 0 }

let flush e =
  Bytes.Writer.write e.writer (Bytes.Slice.make e.o ~first:0 ~length:e.o_next);
  e.o_next <- 0

let encode_eot e = flush e; Bytes.Writer.write_eod e.writer
let encode_char e c =
  if e.o_next > e.o_max then flush e;
  Bytes.set e.o e.o_next c; e.o_next <- e.o_next + 1

let rec encode_uchar e u =
  let rem_len = rem_len e in
  if rem_len < 4 && Uchar.utf_8_byte_length u > rem_len
  then (flush e; encode_uchar e u)
  else (e.o_next <- e.o_next + Bytes.set_utf_8_uchar e.o e.o_next u)

let rec encode_substring e s first length =
  if length = 0 then () else
  let len = Int.min (rem_len e) length in
  if len = 0 then (flush e; encode_substring e s first length) else
  begin
    Stdlib.Bytes.blit_string s first e.o e.o_next len;
    e.o_next <- e.o_next + len;
    encode_substring e s (first + len) (length - len)
  end

let encode_string e s = encode_substring e s 0 (String.length s)

(* Testing *)

let uchars_of_string s =
  let rec loop acc s i max =
    if i > max then List.rev acc else
    let d = String.get_utf_8_uchar s i in
    let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
    loop (u :: acc) s (i + Uchar.utf_decode_length d) max
  in
  loop [] s 0 (String.length s - 1)

let ustr u = Printf.sprintf "U+%04X" u
let strf = Printf.sprintf
let exp exp fnd =
  if (exp <> fnd)
  then failwith (strf "expected %s found: %s" (ustr exp) (ustr fnd))

let rec assert_dec d = function
| [] -> nextc d; exp eot d.u
| u :: us -> nextc d; exp u d.u; assert_dec d us

let test s =
  let uchars = uchars_of_string s in
  for slice_length = 1 to Int.max (String.length s) 1 do
    let d = make_decoder (Bytes.Reader.of_string ~slice_length s) in
    exp sot d.u; assert_dec d uchars
  done

let uchar_utf8 u =
  let b = Bytes.create (Uchar.utf_8_byte_length u) in
  ignore (Bytes.set_utf_8_uchar b 0 u); Bytes.unsafe_to_string b

let test_uchars () =
  let rec loop u =
    if Uchar.equal u Uchar.max then () else
    let s = "abé" ^ (uchar_utf8 u) ^ "🐫" in
    let s' = "abé" ^ (uchar_utf8 u) in
    (test s; test s'; loop (Uchar.succ u))
  in
  loop Uchar.min

let test_encoder () =
  let b = Buffer.create 255 in
  let w = Bytes.Writer.of_buffer b in
  let e = make_encoder ~buf:(Bytes.create 4) w in
  encode_char e 'a';
  encode_uchar e (Uchar.of_int 0x1F42B);
  encode_string e "0123456789";
  encode_eot e;
  assert (Buffer.contents b = "a🐫0123456789");
  ()

let main () =
  B0_testing.Test.main @@ fun () ->
  test "";
  test "a";
  test "abcd";
  test "abécd";
  test "🐫";
  test_uchars ();
  test_encoder ()

let () = if !Sys.interactive then () else exit (main ())
