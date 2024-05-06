/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <blake3.h>

#define blake3_hasher_val(v) (*((blake3_hasher **) Data_custom_val(v)))

/* Library parameters */

CAMLprim value ocaml_bytesrw_blake3_version (value unit)
{ return caml_copy_string (blake3_version ()); }

void ocaml_bytesrw_gc_finalize_blake3_hasher (value hst)
{ caml_stat_free (blake3_hasher_val (hst)); }

CAMLprim value ocaml_bytesrw_blake3_create (value unit)
{
  blake3_hasher *st = caml_stat_alloc (sizeof (blake3_hasher));
  if (!st) caml_failwith ("Could not allocate blake3_hasher");
  value hst = caml_alloc_final (1,
                                &ocaml_bytesrw_gc_finalize_blake3_hasher, 0, 1);
  blake3_hasher_val(hst) = st;
  return hst;
}

CAMLprim value ocaml_bytesrw_blake3_init (value hst)
{ blake3_hasher_init (blake3_hasher_val (hst)); return Val_unit; }

CAMLprim value ocaml_bytesrw_blake3_init_keyed (value hst, value key)
{
  blake3_hasher_init_keyed (blake3_hasher_val (hst), Bytes_val (key));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_blake3_update
(value hst, value str, value ofs, value len)
{
  blake3_hasher_update (blake3_hasher_val (hst),
                        Bp_val (str) + Int_val (ofs),
                        Int_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_blake3_finalize (value hst)
{
  blake3_hasher *st = blake3_hasher_val (hst);
  value res = caml_alloc_string (BLAKE3_OUT_LEN);
  blake3_hasher_finalize (st, Bytes_val (res), BLAKE3_OUT_LEN);
  return res;
}

CAMLprim value ocaml_bytesrw_blake3_hash
(value str, value ofs, value len)
{
  blake3_hasher h;
  uint8_t hash[BLAKE3_OUT_LEN];
  blake3_hasher_init(&h);
  blake3_hasher_update (&h, Bytes_val (str) + Int_val (ofs), Int_val (len));
  blake3_hasher_finalize (&h, hash, BLAKE3_OUT_LEN);
  value res = caml_alloc_initialized_string (BLAKE3_OUT_LEN, (char *)hash);
  return res;
}

CAMLprim value ocaml_bytesrw_blake3_hash_keyed
(value key, value str, value ofs, value len)
{
  blake3_hasher h;
  uint8_t hash[BLAKE3_OUT_LEN];
  blake3_hasher_init_keyed(&h, Bytes_val (key));
  blake3_hasher_update (&h, Bytes_val (str) + Int_val (ofs), Int_val (len));
  blake3_hasher_finalize (&h, hash, BLAKE3_OUT_LEN);
  value res = caml_alloc_initialized_string (BLAKE3_OUT_LEN, (char *)hash);
  return res;
}
