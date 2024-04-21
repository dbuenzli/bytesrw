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
{ return (Val_int (ZSTD_defaultCLevel ())); }

CAMLprim value ocaml_bytesrw_ZSTD_CStreamInSize (value unit)
{ return (Val_int (ZSTD_CStreamInSize ())); }

CAMLprim value ocaml_bytesrw_ZSTD_CStreamOutSize (value unit)
{ return (Val_int (ZSTD_CStreamOutSize ())); }

CAMLprim value ocaml_bytesrw_ZSTD_DStreamInSize (value unit)
{ return (Val_int (ZSTD_DStreamInSize ())); }

CAMLprim value ocaml_bytesrw_ZSTD_DStreamOutSize (value unit)
{ return (Val_int (ZSTD_DStreamOutSize ())); }

/* OCaml Zbuf.t value fields */

#define ocaml_zbuf_bytes 0
#define ocaml_zbuf_size 1
#define ocaml_zbuf_pos 2

/* Decompression */

#define ZSTD_DCtx_val(v) (*((ZSTD_DCtx **) Data_custom_val(v)))

void ocaml_bytesrw_finalize_ZSTD_DCtx (value dctx)
{ size_t rc = ZSTD_freeDCtx (ZSTD_DCtx_val (dctx)); }

CAMLprim value ocaml_bytesrw_ZSTD_createDCtx (value unit)
{
	ZSTD_DCtx *ctx = ZSTD_createDCtx ();
	if (!ctx) caml_failwith ("Could not allocate ZSTD_DCtx");
	value dctx = caml_alloc_final (1, &ocaml_bytesrw_finalize_ZSTD_DCtx, 0, 1);
	ZSTD_DCtx_val(dctx) = ctx;
	return dctx;
}

CAMLprim value ocaml_bytesrw_ZSTD_decompressStream
(value dctx, value src, value dst)
{
	ZSTD_DCtx *ctx = ZSTD_DCtx_val (dctx);
	ZSTD_inBuffer bsrc;
	ZSTD_outBuffer bdst;
	bsrc.src = Bytes_val (Field (src, ocaml_zbuf_bytes));
	bsrc.size = Int_val (Field (src, ocaml_zbuf_size));
	bsrc.pos = Int_val (Field (src, ocaml_zbuf_pos));
	bdst.dst = Bytes_val (Field (dst, ocaml_zbuf_bytes));
	bdst.size = Int_val (Field (dst, ocaml_zbuf_size));
	bdst.pos = Int_val (Field (dst, ocaml_zbuf_pos));

	size_t rc = ZSTD_decompressStream (ctx, &bdst, &bsrc);
	if (rc < 0) caml_failwith (ZSTD_getErrorName (rc));

	Store_field (src, ocaml_zbuf_pos, Val_int (bsrc.pos));
	Store_field (dst, ocaml_zbuf_pos, Val_int (bdst.pos));
  return Val_bool (rc == 0 /* End of frame */);
}
