/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdbool.h>
#include <zstd.h>

#if ZSTD_VERSION_NUMBER < 10400
#error "Unsupported libzstd version, at least 1.4.0 is needed"
#endif

/*
#define Sqlite3_val(v) (*((sqlite3 **) Data_abstract_val(v)))
#define Sqlite3_stmt_val(v) (*((sqlite3_stmt **) Data_abstract_val(v)))
#define Sqlite3_rc_val(v) Int_val(v)
#define Val_sqlite3_rc(v) Val_int(v)
*/

CAMLprim value ocaml_bytesrw_zstd_version (value unit)
{
  return (caml_copy_string (ZSTD_versionString ()));
}

/*
CAMLprim value ocaml_rel_sqlite3_errstr (value rc)
{
  return caml_copy_string (sqlite3_errstr (Sqlite3_rc_val (rc)));
}
*/
