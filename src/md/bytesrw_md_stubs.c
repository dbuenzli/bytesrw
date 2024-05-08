/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#include <sha1.h>
#include <sha2.h>

#define sha1_ctx_val(v) ((SHA1_CTX *)Data_custom_val(v))
#define sha2_ctx_val(v) ((SHA2_CTX *)Data_custom_val(v))

static struct custom_operations bytesrw_md_ops =
{ "bytesrw_md_ops",
  custom_finalize_default, custom_compare_default, custom_compare_ext_default,
  custom_hash_default, custom_serialize_default, custom_deserialize_default };

/* SHA-1 */

CAMLprim value ocaml_bytesrw_sha1_init (value unit)
{
  value hctx = caml_alloc_custom (&bytesrw_md_ops, sizeof (SHA1_CTX), 0, 1);
  SHA1Init (sha1_ctx_val (hctx));
  return hctx;
}

CAMLprim value ocaml_bytesrw_sha1_update
(value hctx, value str, value ofs, value len)
{
  SHA1Update (sha1_ctx_val (hctx), Bytes_val (str) + Int_val (ofs),
              Int_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_sha1_final (value hctx)
{
  /* Make a local copy, SHA1Final updates the context */
  SHA1_CTX ctx = *sha1_ctx_val (hctx);
  value r = caml_alloc_string (SHA1_DIGEST_LENGTH);
  SHA1Final (Bytes_val (r), &ctx);
  return r;
}

CAMLprim value ocaml_bytesrw_sha1_hash
(value str, value ofs, value len)
{
  SHA1_CTX h;
  uint8_t hash[SHA1_DIGEST_LENGTH];
  SHA1Init (&h);
  SHA1Update (&h, Bytes_val (str) + Int_val (ofs), Int_val (len));
  SHA1Final (hash, &h);
  return caml_alloc_initialized_string (SHA1_DIGEST_LENGTH, (char *)hash);
}

/* SHA-256 */

CAMLprim value ocaml_bytesrw_sha256_init (value unit)
{
  value hctx = caml_alloc_custom (&bytesrw_md_ops, sizeof (SHA2_CTX), 0, 1);
  SHA256Init (sha2_ctx_val (hctx));
  return hctx;
}

CAMLprim value ocaml_bytesrw_sha256_update
(value hctx, value str, value ofs, value len)
{
  SHA256Update (sha2_ctx_val (hctx), Bytes_val (str) + Int_val (ofs),
                Int_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_sha256_final (value hctx)
{
  /* Make a local copy, SHA256Final updates the context */
  SHA2_CTX ctx = *sha2_ctx_val (hctx);
  value r = caml_alloc_string (SHA256_DIGEST_LENGTH);
  SHA256Final (Bytes_val (r), &ctx);
  return r;
}

CAMLprim value ocaml_bytesrw_sha256_hash
(value str, value ofs, value len)
{
  SHA2_CTX h;
  uint8_t hash[SHA256_DIGEST_LENGTH];
  SHA256Init (&h);
  SHA256Update (&h, Bytes_val (str) + Int_val (ofs), Int_val (len));
  SHA256Final (hash, &h);
  return caml_alloc_initialized_string (SHA256_DIGEST_LENGTH, (char *)hash);
}

/* SHA-384 */

CAMLprim value ocaml_bytesrw_sha384_init (value unit)
{
  value hctx = caml_alloc_custom (&bytesrw_md_ops, sizeof (SHA2_CTX), 0, 1);
  SHA384Init (sha2_ctx_val (hctx));
  return hctx;
}

CAMLprim value ocaml_bytesrw_sha384_update
(value hctx, value str, value ofs, value len)
{
  SHA384Update (sha2_ctx_val (hctx), Bytes_val (str) + Int_val (ofs),
                Int_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_sha384_final (value hctx)
{
  /* Make a local copy, SHA384Final updates the context */
  SHA2_CTX ctx = *sha2_ctx_val (hctx);
  value r = caml_alloc_string (SHA384_DIGEST_LENGTH);
  SHA384Final (Bytes_val (r), &ctx);
  return r;
}

CAMLprim value ocaml_bytesrw_sha384_hash
(value str, value ofs, value len)
{
  SHA2_CTX h;
  uint8_t hash[SHA384_DIGEST_LENGTH];
  SHA384Init (&h);
  SHA384Update (&h, Bytes_val (str) + Int_val (ofs), Int_val (len));
  SHA384Final (hash, &h);
  return caml_alloc_initialized_string (SHA384_DIGEST_LENGTH, (char *)hash);
}

/* SHA-512 */

CAMLprim value ocaml_bytesrw_sha512_init (value unit)
{
  value hctx = caml_alloc_custom (&bytesrw_md_ops, sizeof (SHA2_CTX), 0, 1);
  SHA512Init (sha2_ctx_val (hctx));
  return hctx;
}

CAMLprim value ocaml_bytesrw_sha512_update
(value hctx, value str, value ofs, value len)
{
  SHA512Update (sha2_ctx_val (hctx), Bytes_val (str) + Int_val (ofs),
                Int_val (len));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_sha512_final (value hctx)
{
  /* Make a local copy, SHA512Final updates the context */
  SHA2_CTX ctx = *sha2_ctx_val (hctx);
  value r = caml_alloc_string (SHA512_DIGEST_LENGTH);
  SHA512Final (Bytes_val (r), &ctx);
  return r;
}

CAMLprim value ocaml_bytesrw_sha512_hash
(value str, value ofs, value len)
{
  SHA2_CTX h;
  uint8_t hash[SHA512_DIGEST_LENGTH];
  SHA512Init (&h);
  SHA512Update (&h, Bytes_val (str) + Int_val (ofs), Int_val (len));
  SHA512Final (hash, &h);
  return caml_alloc_initialized_string (SHA512_DIGEST_LENGTH, (char *)hash);
}
