/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdbool.h>
#include <zlib.h>

#if ZLIB_VERNUM < 0x12b0
#error "Unsupported zlib version, at least 1.2.11 is needed"
#endif


CAMLprim value ocaml_bytesrw_zlib_version (value unit)
{
  return (caml_copy_string (zlibVersion ()));
}
