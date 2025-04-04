/*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/threads.h>

#include <psa/crypto.h>

/* Formally PSA defines psa_status_t as an uint32_t but the ids are small,
   so that should fit on OCaml's ints even on 32-bit platforms. */
#define C_psa_status_t_of_val(v) Long_val(v)
#define C_psa_status_t_to_val(v) Val_long(v)

#define C_psa_key_id_t_of_val(v) (psa_key_id_t)Int32_val(v)
#define C_psa_key_id_t_to_val(v) caml_copy_int32(v)

#define C_psa_algorithm_t_of_val(v) (psa_algorithm_t)Int32_val(v)
#define C_psa_algorithm_t_to_val(v) caml_copy_int32(v)

#define C_psa_key_lifetime_t_of_val(v) (psa_key_lifetime_t)Int32_val(v)
#define C_psa_key_lifetime_t_to_val(v) caml_copy_int32(v)

#define C_psa_key_type_t_of_val(v) (psa_key_type_t)Long_val(v)
#define C_psa_key_type_t_to_val(v) Val_long(v)

#define C_psa_ecc_family_t_of_val(v) (psa_ecc_family_t)Long_val(v)
#define C_psa_ecc_family_t_to_val(v) Val_long(v)

#define C_psa_dh_family_t_of_val(v) (psa_dh_family_t)Long_val(v)
#define C_psa_dh_family_t_to_val(v) Val_long(v)

#define C_psa_key_usage_t_of_val(v) (psa_key_usage_t)Int32_val(v)
#define C_psa_key_usage_t_to_val(v) caml_copy_int32(v)

#define C_psa_key_attributes_t_ptr_of_val(v) \
  (*((psa_key_attributes_t **) Data_custom_val(v)))

#define C_psa_hash_operation_t_ptr_of_val(v) \
  (*((psa_hash_operation_t **) Data_custom_val(v)))

#define C_psa_mac_operation_t_ptr_of_val(v) \
  (*((psa_mac_operation_t **) Data_custom_val(v)))

#define C_psa_cipher_operation_t_ptr_of_val(v) \
  (*((psa_cipher_operation_t **) Data_custom_val(v)))

#define C_psa_aead_operation_t_ptr_of_val(v) \
  (*((psa_aead_operation_t **) Data_custom_val(v)))

#define C_psa_key_derivation_operation_t_ptr_of_val(v) \
  (*((psa_key_derivation_operation_t **) Data_custom_val(v)))

#define bytesrw_slice_length(v) Long_val (Field(v, 2))
#define bytesrw_slice_data(v) \
  ((uint8_t *)(Bytes_val (Field (v, 0))) + Long_val (Field (v, 1)))

#define bytesrw_bigbytes_length(v) Caml_ba_array_val(v)->dim[0]
#define bytesrw_bigbytes_data(v) ((uint8_t *)(Caml_ba_data_val (v)))

CAMLprim value bytesrw_alloc_ok (value v) /* OCaml (Ok v) */
{
  CAMLparam1 (v);
  value ok = caml_alloc_small (1, 0);
  Field (ok, 0) = v;
  CAMLreturn (ok);
}

CAMLprim value bytesrw_alloc_error (value v) /* OCaml (Error e) */
{
  CAMLparam1 (v);
  value err = caml_alloc_small (1, 1);
  Field (err, 0) = v;
  CAMLreturn (err);
}

/* Library management */

CAMLprim value ocaml_bytesrw_psa_crypto_api_version (value unit)
{
  value ret = caml_alloc_tuple (2);
  Store_field (ret, 0, Val_long (PSA_CRYPTO_API_VERSION_MAJOR));
  Store_field (ret, 1, Val_long (PSA_CRYPTO_API_VERSION_MINOR));
  return ret;
}

CAMLprim value ocaml_bytesrw_psa_crypto_init (value unit)
{
  caml_release_runtime_system ();
  psa_status_t rc = psa_crypto_init ();
  caml_acquire_runtime_system ();
  return C_psa_status_t_to_val (rc);
}

/* Key management */

CAMLprim value ocaml_bytesrw_psa_key_type_is_unstructured (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_UNSTRUCTURED
                   (C_psa_key_type_t_of_val (t)));}

CAMLprim value ocaml_bytesrw_psa_key_type_is_asymmetric (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_ASYMMETRIC
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_public_key (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_ASYMMETRIC
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_key_pair (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_ASYMMETRIC
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_rsa (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_RSA
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_ecc_key_pair (value e)
{ return C_psa_key_type_t_to_val (PSA_KEY_TYPE_ECC_KEY_PAIR
                                  (C_psa_ecc_family_t_of_val (e))); }

CAMLprim value ocaml_bytesrw_psa_key_type_ecc_public_key (value e)
{ return C_psa_key_type_t_to_val (PSA_KEY_TYPE_ECC_PUBLIC_KEY
                                  (C_psa_ecc_family_t_of_val (e))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_ecc (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_ECC
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_ecc_key_pair (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_ECC_KEY_PAIR
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_ecc_public_key (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_ECC_PUBLIC_KEY
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_ecc_get_family (value t)
{ return C_psa_ecc_family_t_to_val (PSA_KEY_TYPE_ECC_GET_FAMILY
                                    (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_dh_key_pair (value e)
{ return C_psa_key_type_t_to_val (PSA_KEY_TYPE_DH_KEY_PAIR
                                  (C_psa_dh_family_t_of_val (e))); }

CAMLprim value ocaml_bytesrw_psa_key_type_dh_public_key (value e)
{ return C_psa_key_type_t_to_val (PSA_KEY_TYPE_DH_PUBLIC_KEY
                                  (C_psa_dh_family_t_of_val (e))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_dh (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_DH
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_dh_key_pair (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_DH_KEY_PAIR
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_is_dh_public_key (value t)
{ return Val_bool (PSA_KEY_TYPE_IS_DH_PUBLIC_KEY
                   (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_dh_get_family (value t)
{ return C_psa_dh_family_t_to_val (PSA_KEY_TYPE_DH_GET_FAMILY
                                    (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_key_pair_of_public_key (value t)
{ return C_psa_key_type_t_to_val (PSA_KEY_TYPE_KEY_PAIR_OF_PUBLIC_KEY
                                  (C_psa_key_type_t_of_val (t))); }

CAMLprim value ocaml_bytesrw_psa_key_type_public_key_of_key_pair (value t)
{ return C_psa_key_type_t_to_val (PSA_KEY_TYPE_PUBLIC_KEY_OF_KEY_PAIR
                                  (C_psa_key_type_t_of_val (t))); }

/* Key attributes */

void ocaml_bytesrw_finalize_key_attributes_t (value a)
{
  psa_key_attributes_t *ca = C_psa_key_attributes_t_ptr_of_val (a);
  if (ca != NULL) {
    psa_reset_key_attributes (ca);
    caml_stat_free (ca);
    C_psa_key_attributes_t_ptr_of_val (a) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_psa_key_attributes_init (value unit)
{
  psa_key_attributes_t *ca = caml_stat_alloc (sizeof (psa_key_attributes_t));
  *ca = psa_key_attributes_init ();
  value a = caml_alloc_final(1, &ocaml_bytesrw_finalize_key_attributes_t, 0, 1);
  C_psa_key_attributes_t_ptr_of_val (a) = ca;
  return a;
}

CAMLprim value ocaml_bytesrw_psa_get_key_attributes (value kid, value a)
{
  return C_psa_status_t_to_val
    (psa_get_key_attributes (C_psa_key_id_t_of_val (kid),
                             C_psa_key_attributes_t_ptr_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_reset_key_attributes (value a)
{
  psa_reset_key_attributes (C_psa_key_attributes_t_ptr_of_val (a));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_psa_get_key_algorithm (value a)
{
  return C_psa_algorithm_t_to_val
    (psa_get_key_algorithm (C_psa_key_attributes_t_ptr_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_set_key_algorithm (value a, value alg)
{
  psa_set_key_algorithm (C_psa_key_attributes_t_ptr_of_val (a),
                         C_psa_algorithm_t_of_val (alg));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_psa_get_key_bits (value a)
{
  psa_key_attributes_t *ca = C_psa_key_attributes_t_ptr_of_val (a);
  return Val_long (psa_get_key_bits (ca));
}

CAMLprim value ocaml_bytesrw_psa_set_key_bits (value a, value bits)
{
  psa_set_key_bits (C_psa_key_attributes_t_ptr_of_val (a), Long_val (bits));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_psa_get_key_id (value a)
{
  return C_psa_key_id_t_to_val
    (psa_get_key_id (C_psa_key_attributes_t_ptr_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_set_key_id (value a, value kid)
{
  psa_set_key_id (C_psa_key_attributes_t_ptr_of_val (a),
                  C_psa_key_id_t_of_val (kid));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_psa_get_key_lifetime (value a)
{
  return C_psa_key_lifetime_t_to_val
    (psa_get_key_lifetime (C_psa_key_attributes_t_ptr_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_set_key_lifetime (value a, value l)
{
  psa_set_key_lifetime (C_psa_key_attributes_t_ptr_of_val (a),
                        C_psa_key_lifetime_t_of_val (l));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_psa_get_key_type (value a)
{
  return C_psa_key_type_t_to_val
    (psa_get_key_type (C_psa_key_attributes_t_ptr_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_set_key_type (value a, value t)
{
  psa_set_key_type (C_psa_key_attributes_t_ptr_of_val (a),
                    C_psa_key_type_t_of_val (t));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_psa_get_key_usage_flags (value a)
{
  psa_key_attributes_t *ca = C_psa_key_attributes_t_ptr_of_val (a);
  return C_psa_key_usage_t_to_val (psa_get_key_usage_flags (ca));
}

CAMLprim value ocaml_bytesrw_psa_set_key_usage_flags (value a, value u)
{
  psa_set_key_usage_flags (C_psa_key_attributes_t_ptr_of_val (a),
                           C_psa_key_usage_t_of_val (u));
  return Val_unit;
}

/* Key creation */

CAMLprim value ocaml_bytesrw_psa_import_key (value a, value bb, value len)
{
  psa_key_attributes_t *ca = C_psa_key_attributes_t_ptr_of_val (a);
  uint8_t *cb = bytesrw_bigbytes_data (bb);
  psa_key_id_t ckid = PSA_KEY_ID_NULL;
  psa_status_t st = psa_import_key (ca, cb, Long_val (len), &ckid);
  value ret;
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (C_psa_key_id_t_to_val (ckid));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_generate_key (value a)
{
  psa_key_attributes_t *ca = C_psa_key_attributes_t_ptr_of_val (a);
  psa_key_id_t ckid = PSA_KEY_ID_NULL;
  psa_status_t st = psa_generate_key (ca, &ckid);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (C_psa_key_id_t_to_val (ckid));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_copy_key (value src, value a)
{
  psa_key_id_t csrc = C_psa_key_id_t_of_val (src);
  psa_key_attributes_t *ca = C_psa_key_attributes_t_ptr_of_val (a);
  psa_key_id_t ckid = PSA_KEY_ID_NULL;
  psa_status_t st = psa_copy_key (csrc, ca, &ckid);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (C_psa_key_id_t_to_val (ckid));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

/* Key destruction */

CAMLprim value ocaml_bytesrw_psa_destroy_key (value kid)
{
  return C_psa_status_t_to_val (psa_destroy_key (C_psa_key_id_t_of_val (kid)));
}

CAMLprim value ocaml_bytesrw_psa_purge_key (value kid)
{
  return C_psa_status_t_to_val (psa_purge_key (C_psa_key_id_t_of_val (kid)));
}

/* Key export */

CAMLprim value ocaml_bytesrw_psa_export_key (value kid, value bb)
{
  psa_key_id_t ckid = C_psa_key_id_t_of_val (kid);
  uint8_t *cb = bytesrw_bigbytes_data (bb);
  int blen = bytesrw_bigbytes_length (bb);
  size_t written = 0;
  psa_status_t st = psa_export_key (ckid, cb, blen, &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_export_public_key (value kid, value bb)
{
  psa_key_id_t ckid = C_psa_key_id_t_of_val (kid);
  uint8_t *cb = bytesrw_bigbytes_data (bb);
  int blen = bytesrw_bigbytes_length (bb);
  size_t written = 0;
  psa_status_t st = psa_export_public_key (ckid, cb, blen, &written);
  if (st == PSA_SUCCESS)
    { return bytesrw_alloc_ok (Val_long (written)); }
  else
    { return bytesrw_alloc_error (C_psa_status_t_to_val (st)); }
}

CAMLprim value ocaml_bytesrw_psa_export_key_output_size (value kt, value bits)
{
  return Val_long (PSA_EXPORT_KEY_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt), Long_val (bits)));
}

CAMLprim value ocaml_bytesrw_psa_export_public_key_output_size
(value kt, value bits)
{
  return Val_long (PSA_EXPORT_PUBLIC_KEY_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt), Long_val (bits)));
}

CAMLprim value ocaml_bytesrw_psa_export_key_pair_max_size (value unit)
{ return Val_long (PSA_EXPORT_KEY_PAIR_MAX_SIZE); }

CAMLprim value ocaml_bytesrw_psa_export_public_key_max_size (value unit)
{ return Val_long (PSA_EXPORT_PUBLIC_KEY_MAX_SIZE); }

/* Algorithm categories */

CAMLprim value ocaml_bytesrw_psa_alg_is_hash (value a)
{ return Val_bool (PSA_ALG_IS_HASH (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_mac (value a)
{ return Val_bool (PSA_ALG_IS_MAC (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_cipher (value a)
{ return Val_bool (PSA_ALG_IS_CIPHER (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_aead (value a)
{ return Val_bool (PSA_ALG_IS_AEAD (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_sign (value a)
{ return Val_bool (PSA_ALG_IS_SIGN (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_asymmetric_encryption (value a)
{ return Val_bool (PSA_ALG_IS_ASYMMETRIC_ENCRYPTION
                   (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_key_agreement (value a)
{ return Val_bool (PSA_ALG_IS_KEY_AGREEMENT (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_key_derivation (value a)
{ return Val_bool (PSA_ALG_IS_KEY_DERIVATION (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_wildcard (value a)
{ return Val_bool (PSA_ALG_IS_WILDCARD (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_get_hash (value a)
{
  return C_psa_algorithm_t_to_val (PSA_ALG_GET_HASH
                                   (C_psa_algorithm_t_of_val (a)));
}

/* Message digest */

CAMLprim value ocaml_bytesrw_psa_hash_length (value a)
{ return Val_long (PSA_HASH_LENGTH (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_hash_max_size (value unit)
{ return Val_long (PSA_HASH_MAX_SIZE); }

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_hash_suspend_output_size (value a)
{
  return Val_long (PSA_HASH_SUSPEND_OUTPUT_SIZE
                  (C_psa_algorithm_t_of_val (a)));
}
*/

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_hash_suspend_output_max_size (value unit)
{ return Val_long (PSA_HASH_SUSPEND_OUTPUT_MAX_SIZE); }
*/

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_hash_suspend_algorithm_field_length
(value unit)
{ return Val_long (PSA_HASH_SUSPEND_ALGORITHM_FIELD_LENGTH); }
*/

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_hash_suspend_input_length_field_length
(value a)
{
  return Val_long (PSA_HASH_SUSPEND_INPUT_LENGTH_FIELD_LENGTH
                   (C_psa_algorithm_t_of_val (a)));;
}
*/

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_hash_suspend_hash_state_field_length
(value a)
{
  return Val_long (PSA_HASH_SUSPEND_HASH_STATE_FIELD_LENGTH
                   (C_psa_algorithm_t_of_val (a)));;
}
*/

CAMLprim value ocaml_bytesrw_psa_hash_block_length (value a)
{ return Val_long (PSA_HASH_BLOCK_LENGTH (C_psa_algorithm_t_of_val (a))); }


CAMLprim value ocaml_bytesrw_psa_hash_compute (value a, value i, value h)
{
  size_t written = 0;
  psa_status_t st =
    psa_hash_compute (C_psa_algorithm_t_of_val (a),
                      bytesrw_slice_data (i),
                      bytesrw_slice_length (i),
                      bytesrw_slice_data (h),
                      bytesrw_slice_length (h),
                      &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_hash_compare (value a, value i, value h)

{
  return C_psa_status_t_to_val
    (psa_hash_compare (C_psa_algorithm_t_of_val (a),
                       bytesrw_slice_data (i),
                       bytesrw_slice_length (i),
                       bytesrw_slice_data (h),
                       bytesrw_slice_length (h)));
}

void ocaml_bytesrw_finalize_hash_operation_t (value op)
{
  psa_hash_operation_t *cop = C_psa_hash_operation_t_ptr_of_val (op);
  if (cop != NULL) {
    psa_hash_abort (cop);
    caml_stat_free (cop);
    C_psa_hash_operation_t_ptr_of_val (op) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_psa_hash_operation_init (value unit)
{
  psa_hash_operation_t *cop = caml_stat_alloc (sizeof (psa_hash_operation_t));
  *cop = psa_hash_operation_init ();
  value op = caml_alloc_final(1, &ocaml_bytesrw_finalize_hash_operation_t,0,1);
  C_psa_hash_operation_t_ptr_of_val (op) = cop;
  return op;
}

CAMLprim value ocaml_bytesrw_psa_hash_setup (value op, value alg)
{
  return C_psa_status_t_to_val
    (psa_hash_setup (C_psa_hash_operation_t_ptr_of_val (op),
                     C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_hash_update (value op, value s)
{
  return C_psa_status_t_to_val
    (psa_hash_update (C_psa_hash_operation_t_ptr_of_val (op),
                      bytesrw_slice_data (s),
                      bytesrw_slice_length (s)));
}

CAMLprim value ocaml_bytesrw_psa_hash_finish (value op, value hash)
{
  size_t written = 0;
  psa_status_t st =
    psa_hash_finish (C_psa_hash_operation_t_ptr_of_val (op),
                     bytesrw_slice_data (hash),
                     bytesrw_slice_length (hash),
                     &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_hash_verify (value op, value hash)
{
  return C_psa_status_t_to_val
    (psa_hash_verify (C_psa_hash_operation_t_ptr_of_val (op),
                      bytesrw_slice_data (hash),
                      bytesrw_slice_length (hash)));
}

CAMLprim value ocaml_bytesrw_psa_hash_abort (value op)
{
  return C_psa_status_t_to_val
    (psa_hash_abort (C_psa_hash_operation_t_ptr_of_val (op)));
}

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_hash_suspend (value op, value state)
{
  size_t written = 0;
  psa_status_t st =
  psa_hash_suspend (C_psa_hash_operation_t_ptr_of_val (op),
                    bytesrw_slice_data (state),
                    bytesrw_slice_length (state),
                    &written);

  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}
*/

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_hash_resume (value op, value state)
{
  return C_psa_status_t_to_val
    (psa_hash_resume (C_psa_hash_operation_t_ptr_of_val (op),
                      bytesrw_slice_data (state),
                      bytesrw_slice_length (state)));
}
*/

CAMLprim value ocaml_bytesrw_psa_hash_clone (value src, value dst)
{
  return C_psa_status_t_to_val
    (psa_hash_clone (C_psa_hash_operation_t_ptr_of_val (src),
                     C_psa_hash_operation_t_ptr_of_val (dst)));
}

/* Message authentication codes (MAC) */

CAMLprim value ocaml_bytesrw_psa_alg_hmac (value a)
{
  return C_psa_algorithm_t_to_val (PSA_ALG_HMAC (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_truncated_mac (value a, value length)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_TRUNCATED_MAC (C_psa_algorithm_t_of_val (a),
                            Long_val (length)));
}

CAMLprim value ocaml_bytesrw_psa_alg_full_length_mac (value a)
{
  return C_psa_algorithm_t_to_val (PSA_ALG_FULL_LENGTH_MAC
                                   (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_at_least_this_length_mac
(value a, value length)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_AT_LEAST_THIS_LENGTH_MAC (C_psa_algorithm_t_of_val (a),
                                       Long_val (length)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_hmac (value a)
{ return Val_bool (PSA_ALG_IS_HMAC (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_alg_is_block_cipher_mac (value a)
{
  return Val_bool (PSA_ALG_IS_BLOCK_CIPHER_MAC (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_mac_length (value kt, value bits, value alg)
{
  return Val_long (PSA_MAC_LENGTH (C_psa_key_type_t_of_val (kt),
                                   Long_val (bits),
                                   C_psa_algorithm_t_of_val (alg)));

}

CAMLprim value ocaml_bytesrw_psa_mac_max_size (value unit)
{ return Long_val (PSA_MAC_MAX_SIZE); }

CAMLprim value ocaml_bytesrw_psa_mac_compute
(value kid, value alg, value i, value m)
{
  size_t written = 0;
  psa_status_t st =
    psa_mac_compute (C_psa_key_id_t_of_val (kid),
                     C_psa_algorithm_t_of_val (alg),
                     bytesrw_slice_data (i),
                     bytesrw_slice_length (i),
                     bytesrw_slice_data (m),
                     bytesrw_slice_length (m),
                     &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_mac_verify
(value kid, value alg, value i, value m)
{
  return C_psa_status_t_to_val
    (psa_mac_verify (C_psa_key_id_t_of_val (kid),
                     C_psa_algorithm_t_of_val (alg),
                     bytesrw_slice_data (i),
                     bytesrw_slice_length (i),
                     bytesrw_slice_data (m),
                     bytesrw_slice_length (m)));
}

void ocaml_bytesrw_finalize_mac_operation_t (value op)
{
  psa_mac_operation_t *cop = C_psa_mac_operation_t_ptr_of_val (op);
  if (cop != NULL) {
    psa_mac_abort (cop);
    caml_stat_free (cop);
    C_psa_key_attributes_t_ptr_of_val (op) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_psa_mac_operation_init (value unit)
{
  psa_mac_operation_t *cop = caml_stat_alloc (sizeof (psa_mac_operation_t));
  *cop = psa_mac_operation_init ();
  value op = caml_alloc_final (1, &ocaml_bytesrw_finalize_mac_operation_t,0,1);
  C_psa_mac_operation_t_ptr_of_val (op) = cop;
  return op;
}

CAMLprim value ocaml_bytesrw_psa_mac_sign_setup
(value op, value kid, value alg)
{
  return C_psa_status_t_to_val
    (psa_mac_sign_setup (C_psa_mac_operation_t_ptr_of_val (op),
                         C_psa_key_id_t_of_val (kid),
                         C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_mac_verify_setup
(value op, value kid, value alg)
{
  return C_psa_status_t_to_val
    (psa_mac_verify_setup (C_psa_mac_operation_t_ptr_of_val (op),
                           C_psa_key_id_t_of_val (kid),
                           C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_mac_update (value op, value s)
{
  return C_psa_status_t_to_val
    (psa_mac_update (C_psa_mac_operation_t_ptr_of_val (op),
                     bytesrw_slice_data (s),
                     bytesrw_slice_length (s)));
}

CAMLprim value ocaml_bytesrw_psa_mac_sign_finish (value op, value mac)
{
  size_t written = 0;
  psa_status_t st =
    psa_mac_sign_finish (C_psa_mac_operation_t_ptr_of_val (op),
                         bytesrw_slice_data (mac),
                         bytesrw_slice_length (mac),
                         &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_mac_verify_finish (value op, value mac)
{
  return C_psa_status_t_to_val
    (psa_mac_verify_finish (C_psa_mac_operation_t_ptr_of_val (op),
                            bytesrw_slice_data (mac),
                            bytesrw_slice_length (mac)));
}

CAMLprim value ocaml_bytesrw_psa_mac_abort (value op)
{
  return C_psa_status_t_to_val
    (psa_mac_abort (C_psa_mac_operation_t_ptr_of_val (op)));
}

/* Unauthenticated ciphers */

CAMLprim value ocaml_bytesrw_psa_alg_is_stream_cipher (value a)
{ return Val_bool (PSA_ALG_IS_STREAM_CIPHER (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_cipher_encrypt_output_size
(value kt, value alg, value plain_len)
{
  return Val_long (PSA_CIPHER_ENCRYPT_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt),
                    C_psa_algorithm_t_of_val (alg),
                    Long_val (plain_len)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_encrypt_output_max_size
(value plain_len)
{
  return Val_long (PSA_CIPHER_ENCRYPT_OUTPUT_MAX_SIZE (Long_val (plain_len)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_decrypt_output_size
(value kt, value alg, value cipher_len)
{
  return Val_long (PSA_CIPHER_DECRYPT_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt),
                    C_psa_algorithm_t_of_val (alg),
                    Long_val (cipher_len)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_decrypt_output_max_size
(value cipher_len)
{
  return Val_long (PSA_CIPHER_DECRYPT_OUTPUT_MAX_SIZE (Long_val (cipher_len)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_iv_length (value kt, value alg)
{
  return Val_long (PSA_CIPHER_IV_LENGTH
                   (C_psa_key_type_t_of_val (kt),
                    C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_iv_max_size (value unit)
{ return Val_long (PSA_CIPHER_IV_MAX_SIZE); }


CAMLprim value ocaml_bytesrw_psa_cipher_update_output_size
(value kt, value alg, value ilen)
{
  return Val_long (PSA_CIPHER_UPDATE_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt),
                    C_psa_algorithm_t_of_val (alg),
                    Long_val (ilen)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_update_output_max_size (value ilen)
{
  return Val_long (PSA_CIPHER_UPDATE_OUTPUT_MAX_SIZE (Long_val (ilen)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_finish_output_size (value kt, value alg)
{
  return Val_long (PSA_CIPHER_FINISH_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt),
                    C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_finish_output_max_size (value unit)
{
  return Val_long (PSA_CIPHER_FINISH_OUTPUT_MAX_SIZE);
}

CAMLprim value ocaml_bytesrw_psa_block_cipher_block_length (value kt)
{
  return Val_long (PSA_BLOCK_CIPHER_BLOCK_LENGTH
                   (C_psa_key_type_t_of_val (kt)));
}

CAMLprim value ocaml_bytesrw_psa_block_cipher_block_max_size (value unit)
{
  return Val_long (PSA_BLOCK_CIPHER_BLOCK_MAX_SIZE);
}

CAMLprim value ocaml_bytesrw_psa_cipher_encrypt
(value kid, value alg, value plain, value cipher)
{
  size_t written = 0;
  psa_status_t st =
    psa_cipher_encrypt (C_psa_key_id_t_of_val (kid),
                        C_psa_algorithm_t_of_val (alg),
                        bytesrw_slice_data (plain),
                        bytesrw_slice_length (plain),
                        bytesrw_slice_data (cipher),
                        bytesrw_slice_length (cipher),
                        &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_cipher_decrypt
(value kid, value alg, value cipher, value plain)
{
  size_t written = 0;
  psa_status_t st =
    psa_cipher_decrypt (C_psa_key_id_t_of_val (kid),
                        C_psa_algorithm_t_of_val (alg),
                        bytesrw_slice_data (cipher),
                        bytesrw_slice_length (cipher),
                        bytesrw_slice_data (plain),
                        bytesrw_slice_length (plain),
                        &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

void ocaml_bytesrw_finalize_cipher_operation_t (value op)
{
  psa_cipher_operation_t *cop = C_psa_cipher_operation_t_ptr_of_val (op);
  if (cop != NULL) {
    psa_cipher_abort (cop);
    caml_stat_free (cop);
    C_psa_key_attributes_t_ptr_of_val (op) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_psa_cipher_operation_init (value unit)
{
  psa_cipher_operation_t *cop =
    caml_stat_alloc (sizeof (psa_cipher_operation_t));

  *cop = psa_cipher_operation_init ();
  value op = caml_alloc_final(1, &ocaml_bytesrw_finalize_cipher_operation_t,
                              0, 1);
  C_psa_cipher_operation_t_ptr_of_val (op) = cop;
  return op;
}

CAMLprim value ocaml_bytesrw_psa_cipher_encrypt_setup
(value op, value kid, value alg)
{
  return C_psa_status_t_to_val
    (psa_cipher_encrypt_setup (C_psa_cipher_operation_t_ptr_of_val (op),
                               C_psa_key_id_t_of_val (kid),
                               C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_decrypt_setup
(value op, value kid, value alg)
{
  return C_psa_status_t_to_val
    (psa_cipher_decrypt_setup (C_psa_cipher_operation_t_ptr_of_val (op),
                               C_psa_key_id_t_of_val (kid),
                               C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_generate_iv (value op, value iv)
{
  size_t written = 0;
  psa_status_t st =
    psa_cipher_generate_iv (C_psa_cipher_operation_t_ptr_of_val (op),
                            bytesrw_slice_data (iv),
                            bytesrw_slice_length (iv),
                            &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_cipher_set_iv (value op, value iv)
{
  psa_cipher_operation_t *cop = C_psa_cipher_operation_t_ptr_of_val (op);
  return C_psa_status_t_to_val
    (psa_cipher_set_iv (cop, bytesrw_slice_data (iv),
                        bytesrw_slice_length (iv)));
}

CAMLprim value ocaml_bytesrw_psa_cipher_update
(value op, value input, value output)
{
  size_t written = 0;
  psa_status_t st =
    psa_cipher_update (C_psa_cipher_operation_t_ptr_of_val (op),
                       bytesrw_slice_data (input),
                       bytesrw_slice_length (input),
                       bytesrw_slice_data (output),
                       bytesrw_slice_length (output),
                       &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_cipher_finish (value op, value output)
{
  size_t written = 0;
  psa_status_t st =
    psa_cipher_finish (C_psa_cipher_operation_t_ptr_of_val (op),
                       bytesrw_slice_data (output),
                       bytesrw_slice_length (output),
                       &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_cipher_abort (value op)
{
  return C_psa_status_t_to_val
    (psa_cipher_abort (C_psa_cipher_operation_t_ptr_of_val (op)));
}

/* Authenticated encryption with associated data (AEAD) */

CAMLprim value ocaml_bytesrw_psa_alg_aead_with_shortened_tag
(value a, value length)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_AEAD_WITH_SHORTENED_TAG (C_psa_algorithm_t_of_val (a),
                                      Long_val (length)));
}

CAMLprim value ocaml_bytesrw_psa_alg_aead_with_default_length_tag (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_AEAD_WITH_DEFAULT_LENGTH_TAG (C_psa_algorithm_t_of_val (a)));

}

CAMLprim value ocaml_bytesrw_psa_alg_aead_with_at_least_this_length_tag
(value a, value length)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_AEAD_WITH_AT_LEAST_THIS_LENGTH_TAG (C_psa_algorithm_t_of_val (a),
                                                 Long_val (length)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_aead_on_block_cipher (value a)
{
  return Val_bool
    (PSA_ALG_IS_AEAD_ON_BLOCK_CIPHER (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_aead_encrypt_output_size
(value kt, value alg, value plain_len)
{
  return Val_long (PSA_AEAD_ENCRYPT_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt),
                    C_psa_algorithm_t_of_val (alg),
                    Long_val (plain_len)));
}

CAMLprim value ocaml_bytesrw_psa_aead_encrypt_output_max_size (value plain_len)
{ return Val_long (PSA_AEAD_ENCRYPT_OUTPUT_MAX_SIZE (Long_val (plain_len))); }

CAMLprim value ocaml_bytesrw_psa_aead_decrypt_output_size
(value kt, value alg, value cipher_len)
{
  return Val_long (PSA_AEAD_DECRYPT_OUTPUT_SIZE
                   (C_psa_key_type_t_of_val (kt),
                    C_psa_algorithm_t_of_val (alg),
                    Long_val (cipher_len)));
}

CAMLprim value ocaml_bytesrw_psa_aead_decrypt_output_max_size (value cipher_len)
{ return Val_long (PSA_AEAD_DECRYPT_OUTPUT_MAX_SIZE (Long_val (cipher_len))); }

CAMLprim value ocaml_bytesrw_psa_aead_nonce_length (value kt, value alg)
{
  return Val_long
    (PSA_AEAD_NONCE_LENGTH
     (C_psa_key_type_t_of_val (kt), C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_aead_nonce_max_size (value unit)
{ return Val_long (PSA_AEAD_NONCE_MAX_SIZE); }

CAMLprim value ocaml_bytesrw_psa_aead_update_output_size
(value kt, value alg, value input_len)
{
  return Val_long (PSA_AEAD_UPDATE_OUTPUT_SIZE
                  (C_psa_key_type_t_of_val (kt), C_psa_algorithm_t_of_val (alg),
                   Long_val (input_len)));
}

CAMLprim value ocaml_bytesrw_psa_aead_update_output_max_size (value input_len)
{ return Val_long (PSA_AEAD_UPDATE_OUTPUT_MAX_SIZE (Long_val (input_len))); }

CAMLprim value ocaml_bytesrw_psa_aead_finish_output_size (value kt, value alg)
{
  return Val_long
    (PSA_AEAD_FINISH_OUTPUT_SIZE
     (C_psa_key_type_t_of_val (kt), C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_aead_finish_output_max_size (value unit)
{ return Val_long (PSA_AEAD_FINISH_OUTPUT_MAX_SIZE); }


CAMLprim value ocaml_bytesrw_psa_aead_tag_length
(value kt, value bits, value alg)
{
  return Val_long
    (PSA_AEAD_TAG_LENGTH (C_psa_key_type_t_of_val (kt),
                          Long_val (bits),
                          C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_aead_tag_max_size (value unit)
{ return Val_long (PSA_AEAD_TAG_MAX_SIZE); }

CAMLprim value ocaml_bytesrw_psa_aead_verify_output_size (value kt, value alg)
{
  return Val_long
    (PSA_AEAD_VERIFY_OUTPUT_SIZE
     (C_psa_key_type_t_of_val (kt), C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_aead_verify_output_max_size (value unit)
{ return Val_long (PSA_AEAD_VERIFY_OUTPUT_MAX_SIZE); }


CAMLprim value ocaml_bytesrw_psa_aead_encrypt
(value kid, value alg, value nonce, value ad, value plain, value cipher)
{
  size_t written = 0;
  psa_status_t st =
    psa_aead_encrypt (C_psa_key_id_t_of_val (kid),
                      C_psa_algorithm_t_of_val (alg),
                      bytesrw_slice_data (nonce),
                      bytesrw_slice_length (nonce),
                      bytesrw_slice_data (ad),
                      bytesrw_slice_length (ad),
                      bytesrw_slice_data (plain),
                      bytesrw_slice_length (plain),
                      bytesrw_slice_data (cipher),
                      bytesrw_slice_length (cipher),
                      &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_aead_encrypt_bc (value *argv, int argc)
{
  return ocaml_bytesrw_psa_aead_encrypt
    (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value ocaml_bytesrw_psa_aead_decrypt
(value kid, value alg, value nonce, value ad, value cipher, value plain)
{
  size_t written = 0;
  psa_status_t st =
    psa_aead_decrypt (C_psa_key_id_t_of_val (kid),
                      C_psa_algorithm_t_of_val (alg),
                      bytesrw_slice_data (nonce),
                      bytesrw_slice_length (nonce),
                      bytesrw_slice_data (ad),
                      bytesrw_slice_length (ad),
                      bytesrw_slice_data (cipher),
                      bytesrw_slice_length (cipher),
                      bytesrw_slice_data (plain),
                      bytesrw_slice_length (plain),
                      &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_aead_decrypt_bc (value *argv, int argc)
{
  return ocaml_bytesrw_psa_aead_decrypt
    (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}


void ocaml_bytesrw_finalize_aead_operation_t (value op)
{
  psa_aead_operation_t *cop = C_psa_aead_operation_t_ptr_of_val (op);
  if (cop != NULL) {
    psa_aead_abort (cop);
    caml_stat_free (cop);
    C_psa_key_attributes_t_ptr_of_val (op) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_psa_aead_operation_init (value unit)
{
  psa_aead_operation_t *cop =
    caml_stat_alloc (sizeof (psa_aead_operation_t));

  *cop = psa_aead_operation_init ();
  value op = caml_alloc_final(1, &ocaml_bytesrw_finalize_aead_operation_t,0,1);
  C_psa_aead_operation_t_ptr_of_val (op) = cop;
  return op;
}

CAMLprim value ocaml_bytesrw_psa_aead_encrypt_setup
(value op, value kid, value alg)
{
  return C_psa_status_t_to_val
    (psa_aead_encrypt_setup (C_psa_aead_operation_t_ptr_of_val (op),
                             C_psa_key_id_t_of_val (kid),
                             C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_aead_decrypt_setup
(value op, value kid, value alg)
{
  return C_psa_status_t_to_val
    (psa_aead_decrypt_setup (C_psa_aead_operation_t_ptr_of_val (op),
                             C_psa_key_id_t_of_val (kid),
                             C_psa_algorithm_t_of_val (alg)));
}

CAMLprim value ocaml_bytesrw_psa_aead_set_lengths
(value op, value ad_length, value plain_length)
{
  return C_psa_status_t_to_val
    (psa_aead_set_lengths (C_psa_aead_operation_t_ptr_of_val (op),
                           Long_val (ad_length),
                           Long_val (plain_length)));
}

CAMLprim value ocaml_bytesrw_psa_aead_generate_nonce
(value op, value nonce)
{
  size_t written = 0;
  psa_status_t st =
    psa_aead_generate_nonce (C_psa_aead_operation_t_ptr_of_val (op),
                             bytesrw_slice_data (nonce),
                             bytesrw_slice_length (nonce),
                             &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_aead_set_nonce
(value op, value nonce)
{
  return C_psa_status_t_to_val
    (psa_aead_set_nonce (C_psa_aead_operation_t_ptr_of_val (op),
                         bytesrw_slice_data (nonce),
                         bytesrw_slice_length (nonce)));
}

CAMLprim value ocaml_bytesrw_psa_aead_update_ad
(value op, value ad)
{
  return C_psa_status_t_to_val
    (psa_aead_update_ad (C_psa_aead_operation_t_ptr_of_val (op),
                         bytesrw_slice_data (ad),
                         bytesrw_slice_length (ad)));
}

CAMLprim value ocaml_bytesrw_psa_aead_update
(value op, value input, value output)
{
  size_t written = 0;
  psa_status_t st =
    psa_aead_update (C_psa_aead_operation_t_ptr_of_val (op),
                     bytesrw_slice_data (input),
                     bytesrw_slice_length (input),
                     bytesrw_slice_data (output),
                     bytesrw_slice_length (output),
                     &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_aead_finish
(value op, value cipher, value tag)
{
  size_t written_cipher = 0;
  size_t written_tag = 0;
  psa_status_t st =
    psa_aead_finish (C_psa_aead_operation_t_ptr_of_val (op),
                     bytesrw_slice_data (cipher),
                     bytesrw_slice_length (cipher),
                     &written_cipher,
                     bytesrw_slice_data (tag),
                     bytesrw_slice_length (tag),
                     &written_tag);
  if (st == PSA_SUCCESS)
    {
      value ret = caml_alloc_tuple (2);
      Store_field (ret, 0, Val_long (written_cipher));
      Store_field (ret, 1, Val_long (written_tag));
      return bytesrw_alloc_ok (ret);
    }
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_aead_verify
(value op, value plain, value tag)
{
  size_t written = 0;
  psa_status_t st =
    psa_aead_verify (C_psa_aead_operation_t_ptr_of_val (op),
                     bytesrw_slice_data (plain),
                     bytesrw_slice_length (plain),
                     &written,
                     bytesrw_slice_data (tag),
                     bytesrw_slice_length (tag));
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_aead_abort (value op)
{
  return C_psa_status_t_to_val
    (psa_aead_abort (C_psa_aead_operation_t_ptr_of_val (op)));
}

/* Key derivation */

CAMLprim value ocaml_bytesrw_psa_alg_hkdf (value a)
{
  return C_psa_algorithm_t_to_val (PSA_ALG_HKDF (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_hkdf_extract (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_HKDF_EXTRACT (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_hkdf_expand (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_HKDF_EXPAND (C_psa_algorithm_t_of_val (a)));
}

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_alg_sp800_108_counter_hmac (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_SP800_108_COUNTER_HMAC (C_psa_algorithm_t_of_val (a)));
}
*/

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_alg_sp800_108_counter_cmac (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_SP800_108_COUNTER_CMAC (C_psa_algorithm_t_of_val (a)));
}
*/

CAMLprim value ocaml_bytesrw_psa_alg_tls12_prf (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_TLS12_PRF (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_tls12_psk_to_ms (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_TLS12_PSK_TO_MS (C_psa_algorithm_t_of_val (a)));
}

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_alg_tls12_ecjpake_to_pms (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_TLS12_ECJPAKE_TO_PMS (C_psa_algorithm_t_of_val (a)));
}
*/

CAMLprim value ocaml_bytesrw_psa_alg_pbkdf2_hmac (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_PBKDF2_HMAC (C_psa_algorithm_t_of_val (a)));
}

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_alg_pbkdf2_aes_cmac_prf_128 (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_PBKDF2_AES_CMAC_PRF_128 ));
}
*/

CAMLprim value ocaml_bytesrw_psa_is_key_derivation_stretching (value a)
{
  return Val_bool
    (PSA_ALG_IS_KEY_DERIVATION_STRETCHING (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_is_hkdf (value a)
{ return Val_bool (PSA_ALG_IS_HKDF (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_is_hkdf_extract (value a)
{ return Val_bool (PSA_ALG_IS_HKDF_EXTRACT (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_is_hkdf_expand (value a)
{ return Val_bool (PSA_ALG_IS_HKDF_EXPAND (C_psa_algorithm_t_of_val (a))); }

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_is_sp800_108_counter_hmac (value a)
{
  return Val_bool (PSA_ALG_IS_SP800_108_COUNTER_HMAC
                   (C_psa_algorithm_t_of_val (a)));
}
*/

CAMLprim value ocaml_bytesrw_psa_is_tls12_prf (value a)
{ return Val_bool (PSA_ALG_IS_TLS12_PRF (C_psa_algorithm_t_of_val (a))); }

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_is_tls12_psk_to_ms (value a)
{ return Val_bool (PSA_ALG_IS_TLS12_SK_TO_MS (C_psa_algorithm_t_of_val (a))); }
*/

CAMLprim value ocaml_bytesrw_psa_is_pbkdf2_hmac (value a)
{ return Val_bool (PSA_ALG_IS_PBKDF2_HMAC (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_key_derivation_unlimited_capacity (value unit)
{ return Val_long (PSA_KEY_DERIVATION_UNLIMITED_CAPACITY); }

CAMLprim value ocaml_bytesrw_psa_tls12_psk_to_ms_psk_max_size (value unit)
{ return Val_long (PSA_TLS12_PSK_TO_MS_PSK_MAX_SIZE); }

/* Not in TF-PSA-Crypto 1.0.0
CAMLprim value ocaml_bytesrw_psa_tls12_ecjpake_to_pms_output_size (value unit)
{ return Val_long (TLS12_ECJPAKE_TO_PMS_OUTPUT_SIZE); }
*/

/* OCaml Bytesrw_crypto.Psa.Key_derivation_step value map */
static uint16_t ocaml_bytesrw_psa_key_derivation_step[] =
  { PSA_KEY_DERIVATION_INPUT_SECRET,
    PSA_KEY_DERIVATION_INPUT_OTHER_SECRET,
    PSA_KEY_DERIVATION_INPUT_PASSWORD,
    PSA_KEY_DERIVATION_INPUT_LABEL,
    PSA_KEY_DERIVATION_INPUT_SALT,
    PSA_KEY_DERIVATION_INPUT_INFO,
    PSA_KEY_DERIVATION_INPUT_SEED,
    PSA_KEY_DERIVATION_INPUT_COST,
    /* Not in TF-PSA-Crypto 1.0.0
    PSA_KEY_DERIVATION_INPUT_CONTEXT */
  };

void ocaml_bytesrw_finalize_key_derivation_operation_t (value op)
{
  psa_key_derivation_operation_t *cop =
    C_psa_key_derivation_operation_t_ptr_of_val (op);

  if (cop != NULL) {
    psa_key_derivation_abort (cop);
    caml_stat_free (cop);
    C_psa_key_derivation_operation_t_ptr_of_val (op) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_operation_init (value unit)
{
  psa_key_derivation_operation_t *cop =
    caml_stat_alloc (sizeof (psa_key_derivation_operation_t));

  *cop = psa_key_derivation_operation_init ();
  value op =
    caml_alloc_final(1, &ocaml_bytesrw_finalize_key_derivation_operation_t,
                     0,1);
  C_psa_key_derivation_operation_t_ptr_of_val (op) = cop;
  return op;
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_setup (value op, value a)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_setup (C_psa_key_derivation_operation_t_ptr_of_val (op),
                               C_psa_algorithm_t_of_val(a)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_get_capacity (value op)
{
  size_t cap = 0;
  psa_status_t st =
    psa_key_derivation_get_capacity
    (C_psa_key_derivation_operation_t_ptr_of_val (op), &cap);

  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (cap));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_set_capacity
(value op, value cap)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_set_capacity
     (C_psa_key_derivation_operation_t_ptr_of_val (op), Long_val (cap)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_input_bytes
(value op, value step, value input)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_input_bytes
     (C_psa_key_derivation_operation_t_ptr_of_val (op),
      ocaml_bytesrw_psa_key_derivation_step [Int_val (step)],
      bytesrw_slice_data (input),
      bytesrw_slice_length (input)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_input_integer
(value op, value step, value i)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_input_integer
     (C_psa_key_derivation_operation_t_ptr_of_val (op),
      ocaml_bytesrw_psa_key_derivation_step [Int_val (step)],
      Int64_val (i)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_input_key
(value op, value step, value kid)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_input_key
     (C_psa_key_derivation_operation_t_ptr_of_val (op),
      ocaml_bytesrw_psa_key_derivation_step [Int_val (step)],
      C_psa_key_id_t_of_val (kid)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_output_bytes
(value op, value output)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_output_bytes
     (C_psa_key_derivation_operation_t_ptr_of_val (op),
      bytesrw_slice_data (output),
      bytesrw_slice_length (output)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_output_key
(value katts, value op)
{
  psa_key_id_t ckid = PSA_KEY_ID_NULL;
  psa_status_t st =
    psa_key_derivation_output_key
    (C_psa_key_attributes_t_ptr_of_val(katts),
     C_psa_key_derivation_operation_t_ptr_of_val (op),
     &ckid);

  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (C_psa_key_id_t_to_val (ckid));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_verify_bytes
(value op, value expected)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_verify_bytes
     (C_psa_key_derivation_operation_t_ptr_of_val (op),
      bytesrw_slice_data (expected),
      bytesrw_slice_length (expected)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_verify_key
(value op, value kid)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_verify_key
     (C_psa_key_derivation_operation_t_ptr_of_val (op),
      C_psa_key_id_t_of_val (kid)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_abort (value op)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_abort
     (C_psa_key_derivation_operation_t_ptr_of_val (op)));
}

CAMLprim value ocaml_bytesrw_psa_key_derivation_key_agreement
  (value op, value step, value private_key, value peer_key)
{
  return C_psa_status_t_to_val
    (psa_key_derivation_key_agreement
     (C_psa_key_derivation_operation_t_ptr_of_val (op),
      ocaml_bytesrw_psa_key_derivation_step [Int_val (step)],
      C_psa_key_id_t_of_val (private_key),
      bytesrw_slice_data (peer_key),
      bytesrw_slice_length (peer_key)));
}

/* Asymmetric signature */

CAMLprim value ocaml_bytesrw_psa_alg_rsa_pkcs1v15_sign (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_RSA_PKCS1V15_SIGN (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_rsa_pss (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_RSA_PSS (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_rsa_pss_any_salt (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_RSA_PSS_ANY_SALT (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_ecdsa (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_ECDSA (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_deterministic_ecdsa (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_DETERMINISTIC_ECDSA (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_sign_message (value a)
{
  return Val_bool (PSA_ALG_IS_SIGN_MESSAGE (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_sign_hash (value a)
{
  return Val_bool (PSA_ALG_IS_SIGN_HASH (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_rsa_pkcs1v15_sign (value a)
{
  return Val_bool
    (PSA_ALG_IS_RSA_PKCS1V15_SIGN (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_rsa_pss (value a)
{
  return Val_bool (PSA_ALG_IS_RSA_PSS (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_rsa_pss_any_salt (value a)
{
  return Val_bool (PSA_ALG_IS_RSA_PSS_ANY_SALT (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_rsa_pss_standard_salt (value a)
{
  return Val_bool
    (PSA_ALG_IS_RSA_PSS_STANDARD_SALT (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_ecdsa (value a)
{
  return Val_bool
    (PSA_ALG_IS_ECDSA (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_deterministic_ecdsa (value a)
{
  return Val_bool
    (PSA_ALG_IS_DETERMINISTIC_ECDSA (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_randomized_ecdsa (value a)
{
  return Val_bool
    (PSA_ALG_IS_RANDOMIZED_ECDSA (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_hash_eddsa (value a)
{ return Val_bool (PSA_ALG_IS_HASH_EDDSA (C_psa_algorithm_t_of_val (a))); }


CAMLprim value ocaml_bytesrw_psa_alg_is_hash_and_sign (value a)
{
  return Val_bool (PSA_ALG_IS_HASH_AND_SIGN (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_sign_output_size
(value key_type, value bits, value a)
{
  return Val_long (PSA_SIGN_OUTPUT_SIZE (C_psa_key_type_t_of_val (key_type),
                                         Long_val (bits),
                                         C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_signature_max_size (value unit)
{
  return Val_long (PSA_SIGNATURE_MAX_SIZE);
}

CAMLprim value ocaml_bytesrw_psa_sign_message
(value keyid, value alg, value input, value signature)
{
  size_t written = 0;
  psa_status_t st =
    psa_sign_message (C_psa_key_id_t_of_val (keyid),
                      C_psa_algorithm_t_of_val (alg),
                      bytesrw_slice_data (input),
                      bytesrw_slice_length (input),
                      bytesrw_slice_data (signature),
                      bytesrw_slice_length (signature),
                      &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_verify_message
(value keyid, value alg, value input, value signature)
{
  return C_psa_status_t_to_val
    (psa_verify_message (C_psa_key_id_t_of_val (keyid),
                         C_psa_algorithm_t_of_val (alg),
                         bytesrw_slice_data (input),
                         bytesrw_slice_length (input),
                         bytesrw_slice_data (signature),
                         bytesrw_slice_length (signature)));
}

CAMLprim value ocaml_bytesrw_psa_sign_hash
(value keyid, value alg, value hash, value signature)
{
  size_t written = 0;
  psa_status_t st =
    psa_sign_hash (C_psa_key_id_t_of_val (keyid),
                   C_psa_algorithm_t_of_val (alg),
                   bytesrw_slice_data (hash),
                   bytesrw_slice_length (hash),
                   bytesrw_slice_data (signature),
                   bytesrw_slice_length (signature),
                   &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_verify_hash
(value keyid, value alg, value hash, value signature)
{
  return C_psa_status_t_to_val
    (psa_verify_hash (C_psa_key_id_t_of_val (keyid),
                      C_psa_algorithm_t_of_val (alg),
                      bytesrw_slice_data (hash),
                      bytesrw_slice_length (hash),
                      bytesrw_slice_data (signature),
                      bytesrw_slice_length (signature)));
}

/* Asymmetric encryption */

CAMLprim value ocaml_bytesrw_psa_alg_rsa_oaep (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_RSA_OAEP (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_rsa_oaep (value a)
{ return Bool_val (PSA_ALG_IS_RSA_OAEP (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_asymmetric_encrypt_output_size
(value key_type, value bits, value a)
{
  return Val_long
    (PSA_ASYMMETRIC_ENCRYPT_OUTPUT_SIZE (C_psa_key_type_t_of_val (key_type),
                                         Long_val (bits),
                                         C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_asymmetric_encrypt_output_max_size
(value unit)
{ return Val_long (PSA_ASYMMETRIC_ENCRYPT_OUTPUT_MAX_SIZE); }


CAMLprim value ocaml_bytesrw_psa_asymmetric_decrypt_output_size
(value key_type, value bits, value a)
{
  return Val_long
    (PSA_ASYMMETRIC_DECRYPT_OUTPUT_SIZE (C_psa_key_type_t_of_val (key_type),
                                         Long_val (bits),
                                         C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_asymmetric_decrypt_output_max_size
(value unit)
{ return Val_long (PSA_ASYMMETRIC_DECRYPT_OUTPUT_MAX_SIZE); }

CAMLprim value ocaml_bytesrw_psa_asymmetric_encrypt
(value keyid, value alg, value plain, value salt, value cipher)
{
  size_t written = 0;

  const uint8_t *csalt =
    (Is_none (salt)) ? NULL : bytesrw_slice_data (Some_val (salt));

  size_t csalt_len =
    (Is_none (salt)) ? 0 : bytesrw_slice_length (Some_val (salt));

  psa_status_t st =
    psa_asymmetric_encrypt (C_psa_key_id_t_of_val (keyid),
                            C_psa_algorithm_t_of_val (alg),
                            bytesrw_slice_data (plain),
                            bytesrw_slice_length (plain),
                            csalt, csalt_len,
                            bytesrw_slice_data (cipher),
                            bytesrw_slice_length (cipher),
                            &written);

  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_asymmetric_decrypt
(value keyid, value alg, value cipher, value salt, value plain)
{
  size_t written = 0;

  const uint8_t *csalt =
    (Is_none (salt)) ? NULL : bytesrw_slice_data (Some_val (salt));

  size_t csalt_len =
    (Is_none (salt)) ? 0 : bytesrw_slice_length (Some_val (salt));

  psa_status_t st =
    psa_asymmetric_decrypt (C_psa_key_id_t_of_val (keyid),
                            C_psa_algorithm_t_of_val (alg),
                            bytesrw_slice_data (cipher),
                            bytesrw_slice_length (cipher),
                            csalt, csalt_len,
                            bytesrw_slice_data (plain),
                            bytesrw_slice_length (plain),
                            &written);

  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

/* Key agreement */

CAMLprim value ocaml_bytesrw_psa_alg_key_agreement (value ka, value kdf)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_KEY_AGREEMENT (C_psa_algorithm_t_of_val (ka),
                            C_psa_algorithm_t_of_val (kdf)));
}

CAMLprim value ocaml_bytesrw_psa_alg_key_agreement_get_base (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_KEY_AGREEMENT_GET_BASE (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_key_agreement_get_kdf (value a)
{
  return C_psa_algorithm_t_to_val
    (PSA_ALG_KEY_AGREEMENT_GET_KDF (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_standalone_key_agreement (value a)
{
  return Val_bool
    // Not in TF-PSA-Crypto 1.0.0
    // This is a deprecated name of the same enumerant
    (PSA_ALG_IS_RAW_KEY_AGREEMENT (C_psa_algorithm_t_of_val (a)));
}

CAMLprim value ocaml_bytesrw_psa_alg_is_ffdh (value a)
{ return Val_bool (PSA_ALG_IS_FFDH (C_psa_algorithm_t_of_val (a))); }


CAMLprim value ocaml_bytesrw_psa_alg_is_ecdh (value a)
{ return Val_bool (PSA_ALG_IS_ECDH (C_psa_algorithm_t_of_val (a))); }

CAMLprim value ocaml_bytesrw_psa_raw_key_agreement_output_size
(value kt, value bits)
{
  return Val_long
    (PSA_RAW_KEY_AGREEMENT_OUTPUT_SIZE (C_psa_key_type_t_of_val(kt),
                                        Val_long (bits)));
}

CAMLprim value ocaml_bytesrw_psa_raw_key_agreement_output_max_size (value unit)
{ return Val_long (PSA_RAW_KEY_AGREEMENT_OUTPUT_MAX_SIZE); }

CAMLprim value ocaml_bytesrw_psa_key_agreement
(value private_key, value peer_key, value alg, value key_atts)
{
  psa_key_id_t ckid = PSA_KEY_ID_NULL;
  psa_status_t st =
    psa_key_agreement (C_psa_key_id_t_of_val (private_key),
                       bytesrw_slice_data (peer_key),
                       bytesrw_slice_length (peer_key),
                       C_psa_algorithm_t_of_val (alg),
                       C_psa_key_attributes_t_ptr_of_val (key_atts),
                       &ckid);

  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (C_psa_key_id_t_to_val (ckid));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

CAMLprim value ocaml_bytesrw_psa_raw_key_agreement
(value alg, value private_key, value peer_key, value output)
{
  size_t written = 0;
  psa_status_t st =
    psa_raw_key_agreement (C_psa_algorithm_t_of_val (alg),
                           C_psa_key_id_t_of_val (private_key),
                           bytesrw_slice_data (peer_key),
                           bytesrw_slice_length (peer_key),
                           bytesrw_bigbytes_data (output),
                           bytesrw_bigbytes_length (output),
                           &written);
  if (st == PSA_SUCCESS)
    return bytesrw_alloc_ok (Val_long (written));
  else
    return bytesrw_alloc_error (C_psa_status_t_to_val (st));
}

/* Random number generation */

CAMLprim value ocaml_bytesrw_psa_generate_random (value s)
{
  return C_psa_status_t_to_val
    (psa_generate_random (bytesrw_slice_data (s),
                          bytesrw_slice_length (s)));

}

CAMLprim value ocaml_bytesrw_psa_generate_random_bigbytes (value bb)
{
  return C_psa_status_t_to_val
    (psa_generate_random (bytesrw_bigbytes_data (bb),
                          bytesrw_bigbytes_length (bb)));
}
