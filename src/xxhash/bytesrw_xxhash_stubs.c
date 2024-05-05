/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <xxhash.h>


#define XXH3_state_t_val(v) (*((XXH3_state_t **) Data_custom_val(v)))

/* Library parameters */

CAMLprim value ocaml_bytesrw_XXH_versionNumber (value unit)
{ return Val_int (XXH_versionNumber ());; }

CAMLprim value ocaml_bytesrw_XXH3_64bits_withSeed
(value str, value ofs, value len, value seed)
{
  XXH64_hash_t h =
    XXH3_64bits_withSeed (Bp_val (str) + Int_val (ofs),
                          Int_val (len), Int64_val (seed));
  return (caml_copy_int64(h));
}

/* XXH3_state_t */

void ocaml_bytesrw_finalize_XXH3_state_t (value hst)
{ XXH3_freeState (XXH3_state_t_val (hst)); }

CAMLprim value ocaml_bytesrw_XXH3_createState (value unit)
{
  XXH3_state_t *st = XXH3_createState ();
  if (!st) caml_failwith ("Could not allocate XXH3_state_t");
  value hst = caml_alloc_final (1, &ocaml_bytesrw_finalize_XXH3_state_t, 0, 1);
  XXH3_state_t_val(hst) = st;
  return hst;
}

CAMLprim value ocaml_bytesrw_XXH3_copyState (value dst, value src)
{
	XXH3_copyState (XXH3_state_t_val (dst), XXH3_state_t_val (src));
	return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_64bits_reset (value hst)
{ XXH3_64bits_reset (XXH3_state_t_val (hst)); return Val_unit; }


CAMLprim value ocaml_bytesrw_XXH3_64bits_update
(value hst, value str, value ofs, value len)
{
	XXH3_64bits_update (XXH3_state_t_val (hst),  Bp_val (str) + Int_val (ofs),
											Int_val (len));
	return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_64bits_digest (value hst)
{ return caml_copy_int64 (XXH3_64bits_digest (XXH3_state_t_val (hst))); }
