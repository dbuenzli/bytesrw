(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [deflate], [zlib] and [gzip] compressed bytes.

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc1951}[deflate]},
    {{:https://www.rfc-editor.org/rfc/rfc1950}[zlib]} and
    {{:https://www.rfc-editor.org/rfc/rfc1952}[gzip]} compressed bytes
    with the {{:https://zlib.net/}[zlib]} C library. *)

open Bytesrw

val version : unit -> string
(** [version ()] is the version of the [zlib] C library. *)
