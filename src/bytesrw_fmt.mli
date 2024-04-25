(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val pp_head_hex :
  int -> first:int -> len:int -> Format.formatter -> bytes -> unit

val pp_head_raw :
  int -> first:int -> len:int -> Format.formatter -> bytes -> unit

val pp_raw : first:int -> len:int -> Format.formatter -> bytes -> unit
val pp_hex :
  ?addr:bool -> ?addr_start:int -> ?addr_div:int -> ?count:int ->
  ?group:int -> ?ascii:bool -> ?start:int -> ?len:int -> unit ->
  Format.formatter -> bytes -> unit
(** See {!Bytesrw.Bytes.pp_hex}. *)
