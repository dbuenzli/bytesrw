(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_hex_char ppf i = Format.fprintf ppf "%02x" i
let pp_raw_char ppf c = match Char.code c with
| 0x0A -> Format.pp_print_string ppf "\n"
| 0x0D -> Format.pp_print_string ppf "\r"
| i when i < 0x20 || i = 0x7f -> Format.fprintf ppf "\\x%a" pp_hex_char i
| _ -> (* XXX We should try to decode utf-8 and if not then escape *)
    Format.pp_print_char ppf c

let pp_head_hex count ppf b =
  let max = Int.min count (Bytes.length b) - 1 in
  if max < 0 then Format.pp_print_string ppf "<empty>" else begin
    Format.pp_print_char ppf 'x';
    for i = 0 to max
    do pp_hex_char ppf (Bytes.get_uint8 b i) done;
    if Bytes.length b - 1 > max then Format.fprintf ppf "@<1>%s" "…";
  end

let pp_head_raw count ppf b =
  let max = Int.min count (Bytes.length b) - 1 in
  if max < 0 then Format.pp_print_string ppf "<empty>" else begin
    Format.pp_print_char ppf '\"';
    for i = 0 to max do Format.pp_print_char ppf (Bytes.get b i) done;
    if Bytes.length b - 1 > max then Format.fprintf ppf "@<1>%s" "…";
    Format.pp_print_char ppf '\"';
  end

let pp_raw ~first ~len ppf b =
  Format.pp_open_vbox ppf 1;
  Format.pp_print_char ppf '\"';
  for i = 0 to len - 1 do
    pp_raw_char ppf (Bytes.get b (first + i));
    if (i + 1) mod 60 = 0
    then (Format.pp_print_char ppf '\\'; Format.pp_print_cut ppf ())
  done;
  Format.pp_print_char ppf '\"';
  Format.pp_close_box ppf ()

(* XXX review this *)

let strf = Printf.sprintf
let err_range ~start ~len ~blen =
  invalid_arg @@
  strf "range start %d len %d: not in bounds [0;%d]" start len (blen - 1)

let ilog2 v =
  let rec loop p v = match v with
  | 0 -> p
  | v -> loop (p + 1) (v lsr 1)
  in
  loop (-1) v

let pp_address ~addr ~addr_start ~addr_div ~start ~len =
  let pp_32 ppf addr = Format.fprintf ppf "%08x  " addr in
  let pp_64 ppf addr = Format.fprintf ppf "%016x  " addr in
  if not addr then fun ppf _ -> () else
  let astart = match addr_start with Some a -> a | None -> start in
  let amax = astart + len in
  let pp_address =
    if Sys.int_size = 31 then pp_32 else
    if ilog2 amax < 32 then pp_32 else pp_64
  in
  fun ppf off -> pp_address ppf ((astart + off) / addr_div)

let pp_ascii_col ppf get_uint8 b start stop =
  let pp_ascii_byte ppf b i =
    let byte = get_uint8 b i in
    if byte < 0x1F || byte > 0x7E
    then Format.pp_print_char ppf '.'
    else Format.pp_print_char ppf (Char.chr byte)
  in
  Format.fprintf ppf "  @[<h>@<1>%s" "│";
  for i = start to stop do pp_ascii_byte ppf b i done;
  Format.fprintf ppf "@<1>%s@]" "│";
  ()

let pp_hex
    ?(addr = false) ?addr_start ?(addr_div = 1) ?(count = 16) ?(group = 2)
    ?(ascii = false) ?(start = 0) ?len () ppf b
  =
  let blen = Bytes.length b in
  let len = match len with None -> blen - start | Some len -> len in
  if len = 0 then () else
  let bmax = start + len - 1 in
  match 0 <= start && start <= bmax && bmax < blen with
  | false -> err_range ~start ~len ~blen
  | true ->
      let pp_address = pp_address ~addr ~addr_start ~addr_div ~start ~len in
      Format.pp_open_vbox ppf 0;
      pp_address ppf 0;
      Format.fprintf ppf "%02x" (Bytes.get_uint8 b start);
      for i = start + 1 to bmax do
        if i mod count = 0 then
          begin
            if ascii
            then pp_ascii_col ppf Bytes.get_uint8 b (i - count) (i - 1);
            Format.pp_print_cut ppf ();
            pp_address ppf i
          end
        else if i mod group = 0 then Format.pp_print_char ppf ' ';
        Format.fprintf ppf "%02x" (Bytes.get_uint8 b i);
      done;
      if ascii then begin (* finish the line *)
        for i = bmax + 1 to bmax + (count - (bmax mod count)) - 1 do
          if i mod group = 0 then Format.pp_print_char ppf ' ';
          Format.fprintf ppf "  ";
        done;
        pp_ascii_col ppf Bytes.get_uint8 b  (bmax - bmax mod count) bmax;
      end;
      if addr then (Format.pp_print_cut ppf (); pp_address ppf len);
      Format.pp_close_box ppf ();
      ()
