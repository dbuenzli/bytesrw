/*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdint.h>

#define OCAML_BYTESRW_CRYPTO_RAISE_SYS_ERROR(ERR)                       \
  do { caml_raise_sys_error (caml_copy_string("Bytesrw_crypto: " ERR)); } \
  while (0)

#define bytesrw_slice_length(v) Long_val (Field(v, 2))
#define bytesrw_slice_data(v) \
  ((uint8_t *)(Bytes_val (Field (v, 0))) + Long_val (Field (v, 1)))

#define bytesrw_bigbytes_length(v) Caml_ba_array_val(v)->dim[0]
#define bytesrw_bigbytes_data(v) ((uint8_t *)(Caml_ba_data_val (v)))

/* Except for size_t args this is a copy from tweetnacl.c vn() function.
   Returns 0 if the pointers [x] and [y] are equal for [n] bytes. */
static int verify_n(const uint8_t *x, const uint8_t *y, size_t n)
{
  size_t i = 0;
  unsigned long d = 0;
  for (i = 0 ; i < n; ++i) d |= x[i]^y[i];
  return (1 & ((d - 1) >> 8)) - 1;
}

CAMLprim value ocaml_bytesrw_crypto_verify_equal_slices (value s0, value s1)
{
  size_t len0 = bytesrw_slice_length (s0);
  size_t len1 = bytesrw_slice_length (s1);

  if (len0 != len1)
    caml_invalid_argument_value
      (caml_alloc_sprintf ("Byte slice length mismatch (%zu <> %zu)",
                           len0, len1));

  if (len0 == 0)
    caml_invalid_argument ("Byte slice length must not be 0");

  return Val_bool (!verify_n (bytesrw_slice_data (s0), bytesrw_slice_data (s1),
                              len0));
}

CAMLprim value ocaml_bytesrw_crypto_verify_equal_bigbytes (value b0, value b1)
{
  size_t len0 = bytesrw_bigbytes_length (b0);
  size_t len1 = bytesrw_bigbytes_length (b1);

  if (len0 != len1)
    caml_invalid_argument_value
      (caml_alloc_sprintf ("Bigbytes length mismatch (%zu <> %zu)",
                           len0, len1));

  if (len0 == 0)
    caml_invalid_argument ("Bigbytes length must not be 0");

  return Val_bool (!verify_n (bytesrw_bigbytes_data (b0),
                              bytesrw_bigbytes_data (b1),
                              len0));
}
