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

/* Library parameters */

CAMLprim value ocaml_bytesrw_ZSTD_versionString (value unit)
{ return (caml_copy_string (ZSTD_versionString ())); }

CAMLprim value ocaml_bytesrw_ZSTD_minCLevel (value unit)
{ return (Val_int (ZSTD_minCLevel ())); }

CAMLprim value ocaml_bytesrw_ZSTD_maxCLevel (value unit)
{ return (Val_int (ZSTD_maxCLevel ())); }

CAMLprim value ocaml_bytesrw_ZSTD_defaultCLevel (value unit)
/* Once 1.5.0 is required we can use ZSTD_defaultCLevel () */
{ return (Val_int (ZSTD_CLEVEL_DEFAULT)); }

CAMLprim value ocaml_bytesrw_ZSTD_CStreamInSize (value unit)
{ return (Val_int (ZSTD_CStreamInSize ())); }

CAMLprim value ocaml_bytesrw_ZSTD_CStreamOutSize (value unit)
{ return (Val_int (ZSTD_CStreamOutSize ())); }

CAMLprim value ocaml_bytesrw_ZSTD_DStreamInSize (value unit)
{ return (Val_int (ZSTD_DStreamInSize ())); }

CAMLprim value ocaml_bytesrw_ZSTD_DStreamOutSize (value unit)
{ return (Val_int (ZSTD_DStreamOutSize ())); }

/* OCaml Zbuf.t value fields */

enum ocaml_zbuf_fields
{ ocaml_zbuf_bytes = 0,
  ocaml_zbuf_size,
  ocaml_zbuf_pos };

/* Decompression */

#define ZSTD_DCtx_val(v) (*((ZSTD_DCtx **) Data_custom_val(v)))

void ocaml_bytesrw_finalize_ZSTD_DCtx (value dctx)
{ size_t rc = ZSTD_freeDCtx (ZSTD_DCtx_val (dctx)); /* N.B. accepts NULL */ }

CAMLprim value ocaml_bytesrw_ZSTD_freeDCtx (value dctx)
{
  ZSTD_freeDCtx (ZSTD_DCtx_val (dctx));
  ZSTD_DCtx_val (dctx) = NULL;
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_ZSTD_createDCtx (value unit)
{
  ZSTD_DCtx *ctx = ZSTD_createDCtx ();
  if (!ctx) caml_failwith ("Could not allocate ZSTD_DCtx");
  value dctx = caml_alloc_final (1, &ocaml_bytesrw_finalize_ZSTD_DCtx, 0, 1);
  ZSTD_DCtx_val (dctx) = ctx;
  return dctx;
}

CAMLprim value ocaml_bytesrw_ZSTD_DCtx_setParameter
(value dctx, value param, value v)
{
  ZSTD_DCtx *ctx = ZSTD_DCtx_val (dctx);
  size_t rc = ZSTD_DCtx_setParameter (ctx, Int_val (param), Int_val (v));
  if (ZSTD_isError (rc)) caml_failwith (ZSTD_getErrorName (rc));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_ZSTD_DCtx_loadDictionary(value dctx, value s)
{
  ZSTD_DCtx *ctx = ZSTD_DCtx_val (dctx);
  size_t rc = ZSTD_DCtx_loadDictionary (ctx, String_val (s),
                                        caml_string_length (s));
  if (ZSTD_isError (rc)) caml_failwith (ZSTD_getErrorName (rc));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_ZSTD_decompressStream
(value dctx, value src, value dst)
{
  ZSTD_DCtx *ctx = ZSTD_DCtx_val (dctx);
  ZSTD_inBuffer bsrc;
  bsrc.src = Bytes_val (Field (src, ocaml_zbuf_bytes));
  bsrc.size = Int_val (Field (src, ocaml_zbuf_size));
  bsrc.pos = Int_val (Field (src, ocaml_zbuf_pos));

  ZSTD_outBuffer bdst;
  bdst.dst = Bytes_val (Field (dst, ocaml_zbuf_bytes));
  bdst.size = Int_val (Field (dst, ocaml_zbuf_size));
  bdst.pos = Int_val (Field (dst, ocaml_zbuf_pos));

  size_t rc = ZSTD_decompressStream (ctx, &bdst, &bsrc);
  if (ZSTD_isError (rc)) caml_failwith (ZSTD_getErrorName (rc));

  Store_field (src, ocaml_zbuf_pos, Val_int (bsrc.pos));
  Store_field (dst, ocaml_zbuf_pos, Val_int (bdst.pos));
  return Val_bool (rc == 0 /* End of frame */);
}

/* Compression */

#define ZSTD_CCtx_val(v) (*((ZSTD_CCtx **) Data_custom_val(v)))

void ocaml_bytesrw_finalize_ZSTD_CCtx (value cctx)
{ ZSTD_freeCCtx (ZSTD_CCtx_val (cctx)); /* N.B. accepts NULL */ }

CAMLprim value ocaml_bytesrw_ZSTD_freeCCtx (value cctx)
{
  ZSTD_freeCCtx (ZSTD_CCtx_val (cctx));
  ZSTD_CCtx_val (cctx) = NULL;
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_ZSTD_createCCtx (value unit)
{
  ZSTD_CCtx *ctx = ZSTD_createCCtx ();
  if (!ctx) caml_failwith ("Could not allocate ZSTD_CCtx");
  value cctx = caml_alloc_final (1, &ocaml_bytesrw_finalize_ZSTD_CCtx, 0, 1);
  ZSTD_CCtx_val (cctx) = ctx;
  return cctx;
}

CAMLprim value ocaml_bytesrw_ZSTD_CCtx_setParameter
(value cctx, value param, value v)
{
  ZSTD_CCtx *ctx = ZSTD_CCtx_val (cctx);
  size_t rc = ZSTD_CCtx_setParameter (ctx, Int_val (param), Int_val (v));
  if (ZSTD_isError (rc)) caml_failwith (ZSTD_getErrorName (rc));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_ZSTD_CCtx_loadDictionary(value cctx, value s)
{
  ZSTD_CCtx *ctx = ZSTD_CCtx_val (cctx);
  size_t rc = ZSTD_CCtx_loadDictionary (ctx, String_val (s),
                                        caml_string_length (s));
  if (ZSTD_isError (rc)) caml_failwith (ZSTD_getErrorName (rc));
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_ZSTD_compressStream2
(value dctx, value src, value dst, value end_dir)
{
  ZSTD_CCtx *ctx = ZSTD_CCtx_val (dctx);

  ZSTD_inBuffer bsrc;
  bsrc.src = Bytes_val (Field (src, ocaml_zbuf_bytes));
  bsrc.size = Int_val (Field (src, ocaml_zbuf_size));
  bsrc.pos = Int_val (Field (src, ocaml_zbuf_pos));

  ZSTD_outBuffer bdst;
  bdst.dst = Bytes_val (Field (dst, ocaml_zbuf_bytes));
  bdst.size = Int_val (Field (dst, ocaml_zbuf_size));
  bdst.pos = Int_val (Field (dst, ocaml_zbuf_pos));

  size_t rem = ZSTD_compressStream2 (ctx, &bdst, &bsrc, Int_val (end_dir));
  if (ZSTD_isError (rem)) caml_failwith (ZSTD_getErrorName (rem));
  Store_field (src, ocaml_zbuf_pos, Val_int (bsrc.pos));
  Store_field (dst, ocaml_zbuf_pos, Val_int (bdst.pos));
  return Val_bool (rem == 0 /* end_dir completed */);
}
