/*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_BYTESRW_DARWIN
  #include <sys/random.h>

#elif defined (_WIN32)
  #define OCAML_BYTESRW_WINDOWS
  #include <windows.h>
  # define RtlGenRandom SystemFunction036
  BOOLEAN NTAPI RtlGenRandom(PVOID RandomBuffer, ULONG RandomBufferLength);
  # pragma comment(lib, "advapi32.lib")

#elif defined(__linux__)
  #define OCAML_BYTESRW_LINUX
  #include <sys/random.h>
  #include <unistd.h>
  #include <errno.h>

#elif defined(__unix__) || defined(__unix) /* This should catch the BSDs */
 #include <unistd.h>
 #include <stdlib.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_BYTESRW_POSIX
 #endif
#endif

/* OCaml includes */

#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* getrandom */

#if defined (OCAML_BYTESRW_LINUX)

CAMLprim value ocaml_bytesrw_sysrandom_getrandom
(value b, value first, value length)
{
  uint8_t *sub = ((uint8_t *)(Bytes_val (b))) + Long_val (first);
  size_t rem = (size_t) Long_val (length);
  while (rem > 0)
  {
    ssize_t r = getrandom (sub, rem, 0);
    if (r >= 0) { sub += r; rem -= (size_t) r; }
    else if (errno == EINTR || errno == EAGAIN) continue;
    /* Note normally this should not happen */
    else return caml_alloc_some (caml_copy_string ("getrandom error"));
  }
  return Val_none;
}

#elif (defined (OCAML_BYTESRW_DARWIN) || defined (OCAML_BYTESRW_POSIX))

CAMLprim value ocaml_bytesrw_sysrandom_getrandom
(value b, value first, value length)
{
  uint8_t *sub = ((uint8_t *)(Bytes_val (b))) + Long_val (first);
  arc4random_buf (sub, Long_val (length));
  return Val_none;
}

#elif defined (_WIN32)

CAMLprim value ocaml_bytesrw_sysrandom_getrandom
(value b, value first, value length)
{
  uint8_t *sub = ((uint8_t *)(Bytes_val (b))) + Long_val (first);
  if (RtlGenRandom((PVOID)sub, (ULONG)Long_val (length))) return Val_none;
  else return caml_alloc_some (caml_copy_string ("RtlGenRandom error"));
}


#else /* Unsupported */

#warning ocaml_bytesrw_sysrandom_getrandom: unsupported platform

CAMLprim value ocaml_bytesrw_sysrandom_getrandom
(value b, value first, value length)
{
  return caml_alloc_some
    (caml_copy_string ("getrandom unimplemented on this platform"));
}

#endif

/* getentropy */

#if (defined (OCAML_BYTESRW_LINUX) || defined (OCAML_BYTESRW_DARWIN) || \
     defined (OCAML_BYTESRW_POSIX))

CAMLprim value ocaml_bytesrw_sysrandom_getentropy
(value b, value first, value length)
{
  uint8_t *sub = ((uint8_t *)(Bytes_val (b))) + Long_val (first);
  if (!getentropy (sub, Long_val (length))) return Val_unit;
  else return caml_alloc_some (caml_copy_string("getentropy error"));
}

#elif defined (_WIN32)

CAMLprim value ocaml_bytesrw_sysrandom_getentropy
(value b, value first, value length)
{
  return ocaml_bytesrw_sysrandom_getrandom (b, first, length);
}

#else /* Unsupported */

#warning ocaml_bytesrw_sysrandom_getentropy: unsupported platform

CAMLprim value ocaml_bytesrw_sysrandom_getentropy
(value b, value first, value length)
{
  return caml_alloc_some
    (caml_copy_string ("getentropy unimplemented on this platform"));
}

#endif
