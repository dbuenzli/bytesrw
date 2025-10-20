/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <blake3.h>

#define blake3_hasher_val(v) ((blake3_hasher *)Data_custom_val(v))

static struct custom_operations bytesrw_blake3_ops =
{ "bytesrw_blake3_ops",
  custom_finalize_default, custom_compare_default, custom_compare_ext_default,
  custom_hash_default, custom_serialize_default, custom_deserialize_default };

CAMLprim value ocaml_bytesrw_blake3_version (value unit)
{ return caml_copy_string (blake3_version ()); }

CAMLprim value ocaml_bytesrw_blake3_create (value unit)
{ return caml_alloc_custom (&bytesrw_blake3_ops, sizeof (blake3_hasher), 0, 1);}

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
                        Bytes_val (str) + Long_val (ofs), Long_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_blake3_finalize (value hst)
{
  uint8_t hash[BLAKE3_OUT_LEN];
  blake3_hasher_finalize (blake3_hasher_val (hst), hash, BLAKE3_OUT_LEN);
  return caml_alloc_initialized_string (BLAKE3_OUT_LEN, (char *)hash);
}

CAMLprim value ocaml_bytesrw_blake3_hash
(value str, value ofs, value len)
{
  blake3_hasher h;
  uint8_t hash[BLAKE3_OUT_LEN];
  blake3_hasher_init(&h);
  blake3_hasher_update (&h, Bytes_val (str) + Long_val (ofs), Long_val (len));
  blake3_hasher_finalize (&h, hash, BLAKE3_OUT_LEN);
  return caml_alloc_initialized_string (BLAKE3_OUT_LEN, (char *)hash);
}

CAMLprim value ocaml_bytesrw_blake3_hash_keyed
(value key, value str, value ofs, value len)
{
  blake3_hasher h;
  uint8_t hash[BLAKE3_OUT_LEN];
  blake3_hasher_init_keyed(&h, Bytes_val (key));
  blake3_hasher_update (&h, Bytes_val (str) + Long_val (ofs), Long_val (len));
  blake3_hasher_finalize (&h, hash, BLAKE3_OUT_LEN);
  return caml_alloc_initialized_string (BLAKE3_OUT_LEN, (char *)hash);
}
