/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

/* For the _withSecretandSeed funs, we need this. Also when the minimal
   version is bumped conditionals in these stubs can be removed.  */

#define XXH_PRIVATE_API /* For XXH_CPU_LITTLE_ENDIAN  */
#define XXH_STATIC_LINKING_ONLY

#include <xxhash.h>

#if XXH_VERSION_NUMBER < 800
#error "Unsupported libxxhash version, at least 0.8.0 is needed"
#endif

#define XXH3_state_t_val(v) (*((XXH3_state_t **) Data_custom_val(v)))
#define _BE64(v) (XXH_CPU_LITTLE_ENDIAN ? XXH_swap64(v) : v)

/* Library parameters */

CAMLprim value ocaml_bytesrw_XXH_versionNumber (value unit)
{ return Val_int (XXH_VERSION_NUMBER); }

CAMLprim value ocaml_bytesrw_XXH3_SECRET_SIZE_MIN (value unit)
{ return Val_int (XXH3_SECRET_SIZE_MIN); }

/* Hashing bytes */

CAMLprim value ocaml_bytesrw_XXH3_64bits_withSeed
(value str, value ofs, value len, value seed)
{
  XXH64_hash_t h = XXH3_64bits_withSeed (Bp_val (str) + Int_val (ofs),
                                         Int_val (len), Int64_val (seed));
  return (caml_copy_int64 (h));
}

CAMLprim value ocaml_bytesrw_XXH3_128bits_withSeed
(value str, value ofs, value len, value seed)
{
  XXH128_hash_t h = XXH3_128bits_withSeed (Bp_val (str) + Int_val (ofs),
                                         Int_val (len), Int64_val (seed));
  value res = caml_alloc_string (16);
  ((XXH64_hash_t *)Bp_val(res))[0] = _BE64 (h.high64);
  ((XXH64_hash_t *)(Bp_val(res)))[1] = _BE64 (h.low64);
  return res;
}

/* XXH3_state_t */

static const char ocaml_bytesrw_err_secret_too_small[] = "Secret too small";

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

/* XXH3_64 */

CAMLprim value ocaml_bytesrw_XXH3_64bits_reset (value hst)
{ XXH3_64bits_reset (XXH3_state_t_val (hst)); return Val_unit; }

CAMLprim value ocaml_bytesrw_XXH3_64bits_reset_withSeed
(value hst, value seed)
{
  XXH3_64bits_reset_withSeed (XXH3_state_t_val (hst), Int64_val (seed));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_64bits_reset_withSecret
(value hst, value secret)
{
  XXH_errorcode rc =
    XXH3_64bits_reset_withSecret (XXH3_state_t_val (hst), String_val (secret),
                                  caml_string_length (secret));
  if (rc != XXH_OK) caml_invalid_argument (ocaml_bytesrw_err_secret_too_small);
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_64bits_reset_withSecretandSeed
(value hst, value secret, value seed)
{
#if XXH_VERSION_NUMBER < 801
  caml_raise_sys_error ("Need libxxhash >= 0.8.1 to support secret and hash");
#else
  XXH_errorcode rc =
    XXH3_64bits_reset_withSecretandSeed(XXH3_state_t_val (hst),
                                        String_val (secret),
                                        caml_string_length (secret),
                                        Int64_val (seed));
  if (rc != XXH_OK) caml_invalid_argument (ocaml_bytesrw_err_secret_too_small);
#endif
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_64bits_update
(value hst, value str, value ofs, value len)
{
  XXH3_64bits_update (XXH3_state_t_val (hst),  Bp_val (str) + Int_val (ofs),
                      Int_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_64bits_digest (value hst)
{ return caml_copy_int64 (XXH3_64bits_digest (XXH3_state_t_val (hst))); }

/* XXH3_128 */

CAMLprim value ocaml_bytesrw_XXH3_128bits_reset (value hst)
{ XXH3_128bits_reset (XXH3_state_t_val (hst)); return Val_unit; }

CAMLprim value ocaml_bytesrw_XXH3_128bits_reset_withSeed
(value hst, value seed)
{
  XXH3_128bits_reset_withSeed (XXH3_state_t_val (hst), Int64_val (seed));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_128bits_reset_withSecret
(value hst, value secret)
{
  XXH_errorcode rc =
    XXH3_128bits_reset_withSecret (XXH3_state_t_val (hst), String_val (secret),
                                   caml_string_length (secret));
  if (rc != XXH_OK) caml_invalid_argument (ocaml_bytesrw_err_secret_too_small);
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_128bits_reset_withSecretandSeed
(value hst, value secret, value seed)
{
#if XXH_VERSION_NUMBER < 801
  caml_raise_sys_error ("Need libxxhash >= 0.8.1 to support secret and hash");
#else
  XXH_errorcode rc =
    XXH3_128bits_reset_withSecretandSeed(XXH3_state_t_val (hst),
                                         String_val (secret),
                                         caml_string_length (secret),
                                         Int64_val (seed));
  if (rc != XXH_OK) caml_invalid_argument (ocaml_bytesrw_err_secret_too_small);
#endif
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_128bits_update
(value hst, value str, value ofs, value len)
{
  XXH3_128bits_update (XXH3_state_t_val (hst),  Bp_val (str) + Int_val (ofs),
                       Int_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_XXH3_128bits_digest (value hst)
{
  XXH128_hash_t h = XXH3_128bits_digest (XXH3_state_t_val (hst));
  value res = caml_alloc_string (16);
  ((XXH64_hash_t *)Bp_val(res))[0] = _BE64 (h.high64);
  ((XXH64_hash_t *)(Bp_val(res)))[1] = _BE64 (h.low64);
  return res;
}
