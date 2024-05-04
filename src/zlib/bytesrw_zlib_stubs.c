/*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#include <stdbool.h>
#include <zlib.h>

#if ZLIB_VERNUM < 0x12b0
#error "Unsupported zlib version, at least 1.2.11 is needed"
#endif

#define z_streamp_val(v) (*((z_streamp *) Data_custom_val(v)))

/* Library parameters */

CAMLprim value ocaml_bytesrw_zlib_version (value unit)
{ return (caml_copy_string (zlibVersion ())); }

/* OCaml Bytesrw_zlib.Zbuf.t value fields */

enum ocaml_zbuf_fields
{ ocaml_zbuf_bytes = 0, ocaml_zbuf_size, ocaml_zbuf_pos };

/* OCaml Bytesrw_zlib.flush value map */

static int ocaml_zlib_flush[] =
{ Z_NO_FLUSH, Z_PARTIAL_FLUSH, Z_SYNC_FLUSH, Z_FULL_FLUSH, Z_FINISH,
  Z_BLOCK, Z_TREES };

/* Inflate */

void ocaml_bytesrw_finalize_inflate_z_stream (value zs)
{
  z_streamp s = z_streamp_val (zs);
  if (s != NULL) {
    inflateEnd (s); caml_stat_free (s);
    z_streamp_val (zs) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_free_inflate_z_stream (value zs)
{ ocaml_bytesrw_finalize_inflate_z_stream (zs); return Val_unit; }

CAMLprim value ocaml_bytesrw_create_inflate_z_stream (value window_bits)
{
  z_streamp s = caml_stat_alloc (sizeof (z_stream));
  if (s == NULL) caml_failwith ("Could not allocate z_stream");
  s->zalloc = Z_NULL; s->zfree = Z_NULL; s->opaque = Z_NULL;
  s->avail_in = 0; s->next_in = Z_NULL;

  int rc = inflateInit2(s, Int_val (window_bits));
  if (rc != Z_OK) {
    value err = caml_copy_string( (s->msg) ? s->msg :
                                  "Unknown inflateInit2 error");
    caml_stat_free (s);
    caml_failwith_value (err);
  }
  value zs = caml_alloc_final (1, &ocaml_bytesrw_finalize_inflate_z_stream,
                               0, 1);
  z_streamp_val (zs) = s;
  return zs;
}

CAMLprim value ocaml_bytesrw_inflate_reset (value zs)
{
  z_streamp s = z_streamp_val (zs);
  int rc = inflateReset (s);
  if (rc != Z_OK)
  { caml_failwith ((s->msg) ? s->msg : "Unknown inflateReset error"); }
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_inflate
(value zs, value src, value dst)
{
  z_streamp s = z_streamp_val (zs);

  size_t in_pos = Int_val (Field (src, ocaml_zbuf_pos));
  size_t in_size = Int_val (Field (src, ocaml_zbuf_size));
  s->next_in = Bytes_val (Field (src, ocaml_zbuf_bytes)) + in_pos;
  s->avail_in = in_size - in_pos;

  size_t out_pos = Int_val (Field (dst, ocaml_zbuf_pos));
  size_t out_size = Int_val (Field (dst, ocaml_zbuf_size));
  s->next_out = Bytes_val (Field (dst, ocaml_zbuf_bytes)) + out_pos;
  s->avail_out = out_size - out_pos;

  int rc = inflate (s, Z_NO_FLUSH);
  if (rc != Z_OK && rc != Z_STREAM_END && rc != Z_BUF_ERROR)
  { caml_failwith ((s->msg) ? s->msg : "Unknown inflate error"); }

  size_t in_consumed = in_size - in_pos - s->avail_in;
  size_t out_consumed = out_size - out_pos - s->avail_out;

  Store_field (src, ocaml_zbuf_pos, Val_int (in_pos + in_consumed));
  Store_field (dst, ocaml_zbuf_pos, Val_int (out_pos + out_consumed));

  return Val_bool (rc == Z_STREAM_END);
}

/* Deflate */

void ocaml_bytesrw_finalize_deflate_z_stream (value zs)
{
  z_streamp s = z_streamp_val (zs);
  if (s != NULL) {
    deflateEnd (s); caml_stat_free (s);
    z_streamp_val (zs) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_free_deflate_z_stream (value zs)
{ ocaml_bytesrw_finalize_deflate_z_stream (zs); return Val_unit; }

CAMLprim value ocaml_bytesrw_create_deflate_z_stream
(value level, value window_bits)
{
  z_streamp s = caml_stat_alloc (sizeof (z_stream));
  if (s == NULL) caml_failwith ("Could not allocate z_stream");
  s->zalloc = Z_NULL; s->zfree = Z_NULL; s->opaque = Z_NULL;
  s->avail_in = 0; s->next_in = Z_NULL;

  int rc = deflateInit2(s, Int_val (level), Z_DEFLATED, Int_val (window_bits),
                        8, Z_DEFAULT_STRATEGY);
  if (rc != Z_OK) {
    value err = caml_copy_string( (s->msg) ? s->msg :
                                  "Unknown deflateInit2 error");
    caml_stat_free (s);
    caml_failwith_value (err);
  }
  value zs = caml_alloc_final (1, &ocaml_bytesrw_finalize_deflate_z_stream,
                               0, 1);
  z_streamp_val (zs) = s;
  return zs;
}

CAMLprim value ocaml_bytesrw_deflate_reset (value zs)
{
  z_streamp s = z_streamp_val (zs);
  int rc = deflateReset (s);
  if (rc != Z_OK)
  { caml_failwith ((s->msg) ? s->msg : "Unknown deflateReset error"); }
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_deflate
(value zs, value src, value dst, value flush)
{
  z_streamp s = z_streamp_val (zs);

  size_t in_pos = Int_val (Field (src, ocaml_zbuf_pos));
  size_t in_size = Int_val (Field (src, ocaml_zbuf_size));
  s->next_in = Bytes_val (Field (src, ocaml_zbuf_bytes)) + in_pos;
  s->avail_in = in_size - in_pos;

  size_t out_pos = Int_val (Field (dst, ocaml_zbuf_pos));
  size_t out_size = Int_val (Field (dst, ocaml_zbuf_size));
  s->next_out = Bytes_val (Field (dst, ocaml_zbuf_bytes)) + out_pos;
  s->avail_out = out_size - out_pos;

  int rc = deflate (s, ocaml_zlib_flush [Int_val (flush)]);
  if (rc != Z_OK && rc != Z_STREAM_END && rc != Z_BUF_ERROR)
  { caml_failwith ((s->msg) ? s->msg : "Unknown deflate error"); }

  size_t in_consumed = in_size - in_pos - s->avail_in;
  size_t out_consumed = out_size - out_pos - s->avail_out;

  Store_field (src, ocaml_zbuf_pos, Val_int (in_pos + in_consumed));
  Store_field (dst, ocaml_zbuf_pos, Val_int (out_pos + out_consumed));

  return Val_bool (rc == Z_STREAM_END);
}
