(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Hexadecimal tools.

    See also {!Bytesrw.Bytes.pp_hex}. *)

(** {1:binary_strings Binary strings} *)

val to_binary_string : ?length:int -> string -> (string, string) result
(** [to_binary_string hex] converts [hex], made of upper or lowercase
    US-ASCII hexadecimal digits to a binary string. If [length] is
    specified, errors if the result is not exactly [length] bytes. *)

val of_binary_string : string -> string
(** [of_binary_string s] is the bytes of [s] in lowercase US-ASCII
    hexadecimal digits. *)

val pp_binary_string : Format.formatter -> string -> unit
(** [pp_binary_string ppf s] formats the bytes of [s] with lowercase
    US-ASCII hexadecimal digits. *)

val check_binary_string_length : length:int -> string -> (string, string) result
(** [check_binary_string_length ~length s] checks that [s] has [length]
    bytes and errors otherwise. *)
