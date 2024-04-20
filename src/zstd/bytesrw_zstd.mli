(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [zstd] compressed bytes.

    This module provides support for reading and writing
    {{:https://www.rfc-editor.org/rfc/rfc8878.html}[zstd]} compressed
    bytes with the {{:https://zstd.net/}[libzstd]} C library. *)

open Bytesrw

val version : unit -> string
(** [version ()] is the version of the [libzstd] C library. *)
