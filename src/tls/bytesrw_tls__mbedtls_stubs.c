/*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_BYTESRW_DARWIN

#elif defined (_WIN32)
  #define OCAML_BYTESRW_WINDOWS

#elif defined(__linux__)
  #define OCAML_BYTESRW_LINUX

#elif defined(__unix__) || defined(__unix) /* This should catch the BSDs */
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_BYTESRW_POSIX
 #endif
#endif

/* C stdlib includes */

#include <stdbool.h>
#include <string.h>

/* OCaml bindings includes */

#include <caml/mlvalues.h>
#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

/* PSA crypto */

#include <psa/crypto.h>

// A few things copied over from bytesrw_crypto__psa_stubs.c
#define C_psa_key_id_t_of_val(v) (psa_key_id_t)Int32_val(v)

/* mbedtls includes */

#include <mbedtls/error.h>
#include <mbedtls/base64.h>
#include <mbedtls/pk.h>
#include <mbedtls/ssl.h>

#if MBEDTLS_VERSION_NUMBER < 0x04000000
#error "Unsupported mbedtls version, at least 4.0.0 is needed"
#endif

/* Socket includes */

#if defined(OCAML_BYTESRW_DARWIN) || defined(OCAML_BYTESRW_LINUX) || \
    defined(OCAML_BYTESRW_POSIX)

#include <fcntl.h>
#include <sys/socket.h>
#include <errno.h>

#elif defined(OCAML_BYTESRW_WINDOW)

#include <windows.h>
/* For the fd stuff */
#include <caml/unixsupport.h>

#elif
#error "Could't not determine platform"
#endif

/* System CA store includes */

#if defined(OCAML_BYTESRW_DARWIN)
#include <Security/Security.h>
#include <CoreFoundation/CoreFoundation.h>
#endif

/* mbedtls uses a C int for function return code but the ids are small,
   so that should fit on OCaml's ints even on 32-bit platforms. */

#define C_mbedtls_rc_of_val(v) Int_val(v)
#define C_mbedtls_rc_to_val(v) Val_int(v)

/* OCaml Bytesrw_tls__mbedtls.ssl_protocol_version value map */

static mbedtls_ssl_protocol_version ocaml_mbedtls_ssl_protocol_version[] =
  { MBEDTLS_SSL_VERSION_TLS1_2,
    MBEDTLS_SSL_VERSION_TLS1_3 };

/* OCaml Bytesrw_tls__mbedtls.enum value map */
static int ocaml_mbedtls_enum[] =
  { MBEDTLS_SSL_TRANSPORT_STREAM,
    MBEDTLS_SSL_TRANSPORT_DATAGRAM,
    MBEDTLS_SSL_IS_CLIENT,
    MBEDTLS_SSL_IS_SERVER,
    MBEDTLS_SSL_VERIFY_NONE,
    MBEDTLS_SSL_VERIFY_OPTIONAL,
    MBEDTLS_SSL_VERIFY_REQUIRED,
    MBEDTLS_SSL_PRESET_DEFAULT };

/* Status */

CAMLprim value ocaml_bytesrw_mbedtls_strerror (value rc)
{
  char msg[256];
  mbedtls_strerror (C_mbedtls_rc_of_val (rc), msg, sizeof (msg));
  return caml_copy_string ((char const *)msg);
}

/* Library management */

CAMLprim value ocaml_bytesrw_mbedtls_version (value unit)
{
  value ret = caml_alloc_tuple (3);
  Store_field (ret, 0, Val_long (MBEDTLS_VERSION_MAJOR));
  Store_field (ret, 1, Val_long (MBEDTLS_VERSION_MINOR));
  Store_field (ret, 2, Val_long (MBEDTLS_VERSION_PATCH));
  return ret;
}

/* Base64 encoding */

CAMLprim value ocaml_bytesrw_mbedtls_base64_encode (value src)
{
  CAMLparam1 (src);
  size_t src_len = caml_string_length (src);
  size_t written_len = 0;
  mbedtls_base64_encode (NULL, 0, &written_len, Bytes_val (src), src_len);
  size_t dst_len = written_len;
  value dst = caml_alloc_string (dst_len - 1 /* written_len has final NULL */ );
  mbedtls_base64_encode (Bytes_val (dst), dst_len, &written_len,
                         Bytes_val (src), src_len);
  CAMLreturn (dst);
}

/* mbedtsl_pk_context */

#define C_mbedtls_pk_context_ptr_of_val(v) \
  (*((mbedtls_pk_context **) Data_custom_val(v)))

void ocaml_bytesrw_finalize_pk_context (value pk)
{
  mbedtls_pk_context *cpk = C_mbedtls_pk_context_ptr_of_val (pk);
  if (cpk != NULL) {
    mbedtls_pk_free (cpk);
    caml_stat_free (cpk);
    C_mbedtls_pk_context_ptr_of_val (pk) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_mbedtls_pk_init (value unit)
{
  mbedtls_pk_context *cpk = caml_stat_alloc (sizeof (mbedtls_pk_context));
  mbedtls_pk_init (cpk);
  value pk = caml_alloc_final(1, &ocaml_bytesrw_finalize_pk_context, 0, 1);
  C_mbedtls_pk_context_ptr_of_val (pk) = cpk;
  return pk;
}

CAMLprim value ocaml_bytesrw_mbedtls_pk_parse_keyfile (value pk, value file)
{
  return C_mbedtls_rc_to_val
    (mbedtls_pk_parse_keyfile (C_mbedtls_pk_context_ptr_of_val (pk),
                               String_val (file),
                               NULL));
}

CAMLprim value ocaml_bytesrw_mbedtls_pk_write_keyfile (value pk, value file)
{
  unsigned char pem[4096]; /* Should be enough */
  size_t len = sizeof (pem);
  int rc =
    mbedtls_pk_write_key_pem (C_mbedtls_pk_context_ptr_of_val (pk), pem, len);

  if (rc == 0)
    {
      int pem_len = strlen ((const char *)pem); /* a null byte was written */
      FILE *f = fopen (String_val (file), "wb");
      if (!f) caml_failwith ("Could not open file");
      size_t c = fwrite (pem, 1, pem_len, f);
      fclose (f);
      if (c != pem_len) caml_failwith ("Failed to write all bytes");
    }
  return C_mbedtls_rc_to_val (rc);
}

CAMLprim value ocaml_bytesrw_mbedtls_pk_destroy (value pk)
{
  ocaml_bytesrw_finalize_pk_context (pk);
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_mbedtls_pk_is_valid (value pk)
{
  return Val_bool (C_mbedtls_pk_context_ptr_of_val (pk) != NULL);
}

CAMLprim value ocaml_bytesrw_mbedtls_pk_copy_from_psa (value psa_kid, value pk)
{
  return C_mbedtls_rc_to_val
    (mbedtls_pk_copy_from_psa (C_psa_key_id_t_of_val (psa_kid),
                               C_mbedtls_pk_context_ptr_of_val (pk)));
}

/* mbedtsl_x509_crt */

#define C_mbedtls_x509_crt_ptr_of_val(v) \
  (*((mbedtls_x509_crt **) Data_custom_val(v)))

void ocaml_bytesrw_finalize_mbedtls_x509_crt (value c)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  if (cc != NULL) {
    mbedtls_x509_crt_free (cc);
    caml_stat_free (cc);
    C_mbedtls_x509_crt_ptr_of_val (c) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_mbedtls_x509_crt_init (value unit)
{
  mbedtls_x509_crt *cc = caml_stat_alloc (sizeof (mbedtls_x509_crt));
  mbedtls_x509_crt_init (cc);
  value c = caml_alloc_final(1, &ocaml_bytesrw_finalize_mbedtls_x509_crt, 0, 1);
  C_mbedtls_x509_crt_ptr_of_val (c) = cc;
  return c;
}

CAMLprim value ocaml_bytesrw_mbedtls_x509_crt_parse (value c, value s)
{
  return C_mbedtls_rc_to_val
    (mbedtls_x509_crt_parse (C_mbedtls_x509_crt_ptr_of_val (c),
                             Bytes_val (s),
                             /* Must include NULL for PEM */
                             caml_string_length (s) + 1));
}

CAMLprim value ocaml_bytesrw_mbedtls_x509_crt_parse_der (value c, value s)
{
  return C_mbedtls_rc_to_val
    (mbedtls_x509_crt_parse_der (C_mbedtls_x509_crt_ptr_of_val (c),
                                 Bytes_val (s),
                                 caml_string_length (s)));
}

CAMLprim value ocaml_bytesrw_x509_crt_iter_raw_der_chain (value c, value f)
{
  CAMLparam2 (c, f);
  CAMLlocal1 (der_cert);
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);

  while (cc != NULL)
    {
      der_cert = caml_alloc_initialized_string (cc->raw.len,
                                                (const char *)cc->raw.p);
      caml_callback (f, der_cert);
      cc = cc->next;
    }

  CAMLreturn (Val_unit);
}

CAMLprim value ocaml_bytesrw_x509_crt_get_issuer_name (value c, value b)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  size_t len = caml_string_length (b);
  int ret = mbedtls_x509_dn_gets ((char *)Bytes_val (b), len, &cc->issuer);
  return (ret < 0) ? C_mbedtls_rc_to_val (ret) : Val_long (ret);
}

CAMLprim value ocaml_bytesrw_x509_crt_get_subject_name (value c, value b)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  size_t len = caml_string_length (b);
  int ret = mbedtls_x509_dn_gets ((char *)Bytes_val (b), len, &cc->subject);
  return (ret < 0) ? C_mbedtls_rc_to_val (ret) : Val_long (ret);
}

CAMLprim value ocaml_bytesrw_x509_crt_get_valid_from (value c)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  value ret = caml_alloc_tuple (6);
  Store_field (ret, 0, Val_long (cc->valid_from.year));
  Store_field (ret, 1, Val_long (cc->valid_from.mon));
  Store_field (ret, 2, Val_long (cc->valid_from.day));
  Store_field (ret, 3, Val_long (cc->valid_from.hour));
  Store_field (ret, 4, Val_long (cc->valid_from.min));
  Store_field (ret, 5, Val_long (cc->valid_from.sec));
  return ret;
}

CAMLprim value ocaml_bytesrw_x509_crt_get_valid_to (value c)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  value ret = caml_alloc_tuple (6);
  Store_field (ret, 0, Val_long (cc->valid_to.year));
  Store_field (ret, 1, Val_long (cc->valid_to.mon));
  Store_field (ret, 2, Val_long (cc->valid_to.day));
  Store_field (ret, 3, Val_long (cc->valid_to.hour));
  Store_field (ret, 4, Val_long (cc->valid_to.min));
  Store_field (ret, 5, Val_long (cc->valid_to.sec));
  return ret;
}

CAMLprim value ocaml_bytesrw_x509_crt_get_serial (value c)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  return caml_alloc_initialized_string (cc->serial.len,
                                        (const char *)cc->serial.p);
}

CAMLprim value ocaml_bytesrw_x509_crt_get_is_ca (value c, value b)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  return Val_bool (cc->private_ca_istrue);
}

CAMLprim value ocaml_bytesrw_x509_crt_generate
(value c, value invalid_before, value invalid_after, value is_ca,
 value issuer, value issuer_key, value subject, value subject_key,
 value subject_alt_dns)
{
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);
  mbedtls_pk_context *cikey = C_mbedtls_pk_context_ptr_of_val (issuer_key);
  mbedtls_pk_context *cskey = C_mbedtls_pk_context_ptr_of_val (subject_key);
  int rc = 0;
  mbedtls_x509write_cert w;
  uint8_t serial[8]; /* random 64 bits */

  psa_generate_random (serial, sizeof(serial));
  mbedtls_x509write_crt_init (&w);
  mbedtls_x509write_crt_set_md_alg (&w, MBEDTLS_MD_SHA256);
  mbedtls_x509write_crt_set_issuer_key (&w, cikey);
  mbedtls_x509write_crt_set_subject_key (&w, cskey);
  if ((rc = mbedtls_x509write_crt_set_issuer_name (&w, String_val (issuer))))
    return C_mbedtls_rc_to_val (rc);;
  if ((rc = mbedtls_x509write_crt_set_subject_name (&w, String_val (subject))))
    return C_mbedtls_rc_to_val (rc);;
  if ((rc = mbedtls_x509write_crt_set_serial_raw (&w, serial, sizeof(serial))))
    return C_mbedtls_rc_to_val (rc);;
  if ((rc = mbedtls_x509write_crt_set_validity
       (&w, String_val (invalid_before), String_val (invalid_after))))
    return C_mbedtls_rc_to_val (rc);
  if (Bool_val (is_ca))
    if ((rc = mbedtls_x509write_crt_set_basic_constraints (&w, 1, -1)))
      return C_mbedtls_rc_to_val (rc);;

  if (caml_string_length (subject_alt_dns) > 0) {
      mbedtls_x509_buf buf;
      mbedtls_x509_subject_alternative_name name;
      mbedtls_x509_san_list san_list;
      buf.len = caml_string_length (subject_alt_dns);
      buf.p = Bytes_val (subject_alt_dns);
      name.type = MBEDTLS_X509_SAN_DNS_NAME;
      name.san.unstructured_name = buf;
      san_list.node = name;
      san_list.next = NULL;
      if ((rc = mbedtls_x509write_crt_set_subject_alternative_name
           (&w, &san_list)))
        return C_mbedtls_rc_to_val (rc);;
  }

  unsigned char der[4096]; /* Should be enough */
  size_t der_len = sizeof (der);
  rc = mbedtls_x509write_crt_der (&w, der, der_len);
  if (rc < 0) return C_mbedtls_rc_to_val (rc);

  /* Data is written at the end of the buffer */
  const unsigned char *der_start = der + der_len - rc;
  rc = mbedtls_x509_crt_parse_der (cc, der_start, rc);
  return C_mbedtls_rc_to_val (rc);
}

CAMLprim value ocaml_bytesrw_x509_crt_generate_bc
(value * argv, int argn)
{
  return ocaml_bytesrw_x509_crt_generate
    (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6], argv[7],
     argv[8]);
}

#if defined (OCAML_BYTESRW_WINDOWS)
CAMLprim value ocaml_bytesrw_win_x509_crt_of_system_store (value unit)
{
  CAMLparam0 ();
  CAMLlocal1 (c);
  c = ocaml_bytesrw_mbedtls_x509_crt_init (Val_unit);
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);

  HCERTSTORE store = CertOpenSystemStore (0, "CA"); /* or ROOT ? */ ;
  if (!store) caml_failwith ("CertOpenSystemStore failed");

  PCCERT_CONTEXT cert = NULL;
  while ((cert = CertEnumCertificatesInStore(store, cert)))
    {
      if ((cert->dwCertEncodingType & X509_ASN_ENCODING /* DER */ ))
        {
          const unsigned char *enc = cert->pbCertEncoded;
          const int len = cert->cbCertEncoded;
          int rc = mbedtls_x509_crt_parse_der (cc, enc, len);
          if (rc < 0)
            {
              CertCloseStore (store, 0);
              caml_failwith
                ("mbedtsl failed to parse DER system store certificate");
            }
        }
      else
        {
          // XXX Should we rather skip?
          CertCloseStore (store, 0);
          caml_failwith ("mbedtsl can't read that system store certificate");
        }
    }
  CertCloseStore (store, 0);
  CAMLreturn (caml_alloc_some (c));
}
#endif

#if defined (OCAML_BYTESRW_DARWIN)
CAMLprim value ocaml_bytesrw_x509_crt_of_system_store_darwin (value unit)
{
  CAMLparam0 ();
  CAMLlocal1 (c);
  c = ocaml_bytesrw_mbedtls_x509_crt_init (Val_unit);
  mbedtls_x509_crt *cc = C_mbedtls_x509_crt_ptr_of_val (c);

  SecTrustSettingsDomain domains[] =
    { kSecTrustSettingsDomainSystem,
      kSecTrustSettingsDomainAdmin,
      kSecTrustSettingsDomainUser };

  for (size_t d = 0; d < sizeof(domains) / sizeof(domains[0]); d++) {
    CFArrayRef certs = NULL;
    OSStatus err = SecTrustSettingsCopyCertificates (domains[d], &certs);

    if (err == errSecNoTrustSettings)
      {
        if (certs != NULL) CFRelease (certs);
        break;
      }

    if (err != errSecSuccess || certs == NULL)
      {
        if (certs != NULL) CFRelease (certs);
        caml_failwith ("SecTrustSettingsCopyCertificates failed");
      }

    CFIndex count = CFArrayGetCount (certs);
    for (CFIndex i = 0; i < count; ++i) {
      CFDataRef der =
        SecCertificateCopyData
        ((SecCertificateRef)CFArrayGetValueAtIndex(certs, i));

      const UInt8 *data = CFDataGetBytePtr(der);
      CFIndex len   = CFDataGetLength(der);

      int rc = mbedtls_x509_crt_parse_der (cc, data, len);
      if (rc < 0)
        {
          CFRelease (der);
          CFRelease (certs);
          caml_failwith
            ("mbedtsl failed to parse DER system store certificate");
      }
      CFRelease (der);
    }
    CFRelease(certs);
  }
  CAMLreturn (caml_alloc_some (c));
}
#endif

CAMLprim value ocaml_bytesrw_x509_crt_of_system_store (value unit)
{
#if defined (OCAML_BYTESRW_WINDOWS)
  return ocaml_bytesrw_x509_crt_of_system_store_windows (unit);
#elif defined (OCAML_BYTESRW_DARWIN)
  return ocaml_bytesrw_x509_crt_of_system_store_darwin (unit);
#else
  // We could try to lookup the keychain on macOS but for
  // now we are happy to just lookup /etc/ssl/cert.pem
  // note that it is used by the system distributed curl.
  return Val_unit;
#endif
}

/* mbedtls_ssl_config */

#define C_alpn_protocols_ptr_of_val(v) \
  (*((char * **) Data_custom_val(v)))

void ocaml_bytesrw_finalize_alpn_protocols (value ps)
{
  char * *cps = C_alpn_protocols_ptr_of_val (ps);
  if (cps != NULL)
    {
      for (size_t i = 0; cps[i] != NULL; i++) caml_stat_free ((void *)cps[i]);
      caml_stat_free ((void *)cps);
      C_alpn_protocols_ptr_of_val (ps) = NULL;
    }
}

CAMLprim value ocaml_bytesrw_alpn_protocols_init (value ps)
{
  int ps_len = Wosize_val (ps);
  char * *cps = caml_stat_alloc ((ps_len + 1) * sizeof (char *));
  cps[ps_len] = NULL;

  for (size_t i = 0; i < ps_len; i++)
    cps[i] = caml_stat_strdup (String_val (Field (ps, i)));

  value v = caml_alloc_final (1, &ocaml_bytesrw_finalize_alpn_protocols, 0, 1);
  C_alpn_protocols_ptr_of_val (v) = cps;
  return v;
}

#define C_mbedls_ssl_config_ptr_of_val(v) \
  (*((mbedtls_ssl_config **) Data_custom_val(v)))

void ocaml_bytesrw_finalize_mbedtls_ssl_config (value c)
{
  mbedtls_ssl_config *cc = C_mbedls_ssl_config_ptr_of_val (c);
  if (cc != NULL) {
    mbedtls_ssl_config_free (cc);
    caml_stat_free (cc);
    C_mbedls_ssl_config_ptr_of_val (c) = NULL;
  }
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_config_init (value unit)
{
  mbedtls_ssl_config *cc = caml_stat_alloc (sizeof (mbedtls_ssl_config));
  mbedtls_ssl_config_init (cc);
  value c = caml_alloc_final (1, &ocaml_bytesrw_finalize_mbedtls_ssl_config,
                              0, 1);
  C_mbedls_ssl_config_ptr_of_val (c) = cc;
  return c;
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_config_defaults
(value c, value who, value transport, value preset)
{
  return C_mbedtls_rc_to_val
    (mbedtls_ssl_config_defaults (C_mbedls_ssl_config_ptr_of_val (c),
                                  ocaml_mbedtls_enum [Int_val (who)],
                                  ocaml_mbedtls_enum [Int_val (transport)],
                                  ocaml_mbedtls_enum [Int_val (preset)]));
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_conf_authmode
(value c, value mode)
{
  mbedtls_ssl_conf_authmode (C_mbedls_ssl_config_ptr_of_val (c),
                             ocaml_mbedtls_enum [Int_val (mode)]);
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_conf_ca_chain
(value c, value chain)
{
  mbedtls_ssl_conf_ca_chain (C_mbedls_ssl_config_ptr_of_val (c),
                             C_mbedtls_x509_crt_ptr_of_val (chain),
                             NULL);
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_conf_own_cert
(value c, value chain, value pk)
{
  return C_mbedtls_rc_to_val
    (mbedtls_ssl_conf_own_cert (C_mbedls_ssl_config_ptr_of_val (c),
                                C_mbedtls_x509_crt_ptr_of_val (chain),
                                C_mbedtls_pk_context_ptr_of_val (pk)));
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_conf_min_tls_version
(value c, value version)
{
  mbedtls_ssl_conf_min_tls_version
    (C_mbedls_ssl_config_ptr_of_val (c),
     ocaml_mbedtls_ssl_protocol_version[Int_val (version)]);
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_conf_max_tls_version
(value c, value version)
{
  mbedtls_ssl_conf_max_tls_version
    (C_mbedls_ssl_config_ptr_of_val (c),
     ocaml_mbedtls_ssl_protocol_version[Int_val (version)]);
  return Val_unit;
}

CAMLprim value ocaml_byterw_mbedtls_ssl_conf_alpn_protocols
(value c, value ps)
{
  return C_mbedtls_rc_to_val
    (mbedtls_ssl_conf_alpn_protocols
     (C_mbedls_ssl_config_ptr_of_val (c),
      (const char * *)C_alpn_protocols_ptr_of_val (ps)));
}

/* mbedtls_ssl_context */

#define C_mbedls_ssl_context_ptr_of_val(v) \
  (*((mbedtls_ssl_context **) Data_custom_val(v)))

CAMLprim value ocaml_bytesrw_mbedtls_ssl_context_destroy (value c)
{
  mbedtls_ssl_context *cc = C_mbedls_ssl_context_ptr_of_val (c);
  if (cc != NULL) {
    mbedtls_ssl_free (cc);
    caml_stat_free (cc);
    C_mbedls_ssl_context_ptr_of_val (c) = NULL;
  }
  return Val_unit;
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_context_init (value unit)
{
  mbedtls_ssl_context *cc = caml_stat_alloc (sizeof (mbedtls_ssl_context));
  mbedtls_ssl_init (cc);
  value c = caml_alloc_final(1, NULL, 0, 1);
  C_mbedls_ssl_context_ptr_of_val (c) = cc;
  return c;
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_setup (value ctx, value conf)
{
  return C_mbedtls_rc_to_val
    (mbedtls_ssl_setup (C_mbedls_ssl_context_ptr_of_val (ctx),
                        C_mbedls_ssl_config_ptr_of_val (conf)));
}

CAMLprim value ocaml_bytesrw_mbedtls_ssl_set_hostname (value ctx, value h)
{
  if (!caml_string_is_c_safe (h))
    caml_invalid_argument
      ("bytesrw_mbedtls_ssl_set_hostname: string is not C safe.");
  return C_mbedtls_rc_to_val
    (mbedtls_ssl_set_hostname (C_mbedls_ssl_context_ptr_of_val (ctx),
                               String_val (h)));
}

static bool socket_is_non_blocking (int fd)
{ return (bool)((fcntl (fd, F_GETFL) & O_NONBLOCK)); }

static int socket_non_blocking_send
(void *ctx, const unsigned char *buf, size_t len)
{
#if defined(OCAML_BYTESRW_WINDOW)
  SOCKET fd = (SOCKET)ctx;
#else
  int fd = (int)(intptr_t)ctx;
#endif
  int ret = send (fd, buf, len, 0);
  if (ret >= 0) return ret;
  else if (errno == EINTR || errno == EWOULDBLOCK)
    return MBEDTLS_ERR_SSL_WANT_WRITE;
  else return -1;
}

static int socket_non_blocking_recv
(void *ctx, unsigned char *buf, size_t len)
{
#if defined(OCAML_BYTESRW_WINDOW)
  SOCKET fd = (SOCKET)ctx;
#else
  int fd = (int)(intptr_t)ctx;
#endif
  int ret = recv (fd, buf, len, 0);
  if (ret >= 0) return ret;
  else if (errno == EINTR || errno == EWOULDBLOCK)
    return MBEDTLS_ERR_SSL_WANT_READ;
  return -1;
}

static int socket_blocking_send
(void *ctx, const unsigned char *buf, size_t len)
{
#if defined(OCAML_BYTESRW_WINDOW)
  SOCKET fd = (SOCKET)ctx;
#else
  int fd = (int)(intptr_t)ctx;
#endif
  int ret = send (fd, buf, len, 0);
  if (ret >= 0) return ret;
  else if (errno == EINTR) return socket_blocking_send (ctx, buf, len);
  else return -1;
}

static int socket_blocking_recv
(void *ctx, unsigned char *buf, size_t len, uint32_t timeout_ms)
{
#if defined(OCAML_BYTESRW_WINDOW)
  SOCKET fd = (SOCKET)ctx;
#else
  int fd = (int)(intptr_t)ctx;
#endif
  struct timeval tv = { 0 };
  tv.tv_sec  = timeout_ms / 1000;
  tv.tv_usec = (timeout_ms % 1000) * 1000;
  int ret = setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO,
                       (const char*)&tv, sizeof tv);
  if (ret < 0) return -1;
  ret = recv (fd, buf, len, 0);
  if (ret >= 0) return ret;
  if (errno == EINTR) return MBEDTLS_ERR_SSL_WANT_READ; /* XXX unclear */
  else if (errno == ETIMEDOUT) return MBEDTLS_ERR_SSL_TIMEOUT;
  else return -1;
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_set_bio_to_socket_fd (value ctx, value fd)
{
  mbedtls_ssl_context *cctx = C_mbedls_ssl_context_ptr_of_val (ctx);

#if defined(OCAML_BYTESRW_WINDOW)
  if (Descr_kind_val(fd) != KIND_SOCKET)
    caml_invalid_argument ("File descriptor is not a socket");
  SOCKET cfd = Socket_val (fd);
  void* ioctx = (void *)cfd;
#else
  int cfd = Int_val (fd);
  void* ioctx = (void *)(intptr_t)(cfd); /* Use the FD directly */
#endif

  if (socket_is_non_blocking (cfd))
    mbedtls_ssl_set_bio (cctx, ioctx, socket_non_blocking_send,
                         socket_non_blocking_recv, NULL);
  else
    mbedtls_ssl_set_bio (cctx, ioctx, socket_blocking_send, NULL,
                         socket_blocking_recv);
  return Val_unit;
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_handshake (value ctx)
{
  mbedtls_ssl_context *cctx = C_mbedls_ssl_context_ptr_of_val (ctx);
  int rc;
  caml_release_runtime_system ();
  rc = mbedtls_ssl_handshake (cctx);
  while (rc == 0 && !mbedtls_ssl_is_handshake_over (cctx))
    { rc = mbedtls_ssl_handshake_step (cctx); }
  caml_acquire_runtime_system ();
  return C_mbedtls_rc_to_val (rc);
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_get_version_number (value ctx)
{
  mbedtls_ssl_protocol_version v =
    mbedtls_ssl_get_version_number (C_mbedls_ssl_context_ptr_of_val (ctx));
  if (v == MBEDTLS_SSL_VERSION_TLS1_2) return caml_alloc_some (Val_long (0));
  if (v == MBEDTLS_SSL_VERSION_TLS1_3) return caml_alloc_some (Val_long (1));
  else return Val_none;
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_get_alpn_protocol (value ctx)
{
  const char *alpn =
    mbedtls_ssl_get_alpn_protocol (C_mbedls_ssl_context_ptr_of_val (ctx));

  if (!alpn) return (Val_none);
  else return (caml_alloc_some (caml_copy_string (alpn)));
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_get_peer_cert (value ctx)
{
  const mbedtls_x509_crt *src =
    mbedtls_ssl_get_peer_cert (C_mbedls_ssl_context_ptr_of_val (ctx));

  if (!src) return Val_none;
  else {
    value ocert = ocaml_bytesrw_mbedtls_x509_crt_init (Val_unit);
    mbedtls_x509_crt *dst = C_mbedtls_x509_crt_ptr_of_val (ocert);
    while (src != NULL)
    {
      mbedtls_x509_crt_parse (dst, src->raw.p, src->raw.len);
      src = src->next;
    }
    return (caml_alloc_some (ocert));
  }
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_read (value ctx, value buf, value first, value len)
{
  CAMLparam1(buf);
  mbedtls_ssl_context *cctx = C_mbedls_ssl_context_ptr_of_val (ctx);
  long clen = Long_val (len);
  unsigned char iobuf[UNIX_BUFFER_SIZE];

  if (clen > UNIX_BUFFER_SIZE) clen = UNIX_BUFFER_SIZE;

  caml_release_runtime_system ();
  int rc = mbedtls_ssl_read (cctx, iobuf, clen);
  caml_acquire_runtime_system ();

  if (rc == 0 || rc == MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY)
    CAMLreturn (Val_long (0));
  else if (rc == MBEDTLS_ERR_SSL_WANT_READ ||
           rc == MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET)
    CAMLreturn (ocaml_bytesrw_mbedtls_ssl_read (ctx, buf, first, len));
  else if (rc < 0)
    CAMLreturn (C_mbedtls_rc_to_val (rc));
  else {
    memcpy (&Byte (buf, Long_val (first)), iobuf, rc);
    CAMLreturn (Val_long (rc));
  }
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_write (value ctx, value buf, value first, value len)
{
  mbedtls_ssl_context *cctx = C_mbedls_ssl_context_ptr_of_val (ctx);
  long clen = Long_val (len);
  unsigned char iobuf[UNIX_BUFFER_SIZE];

  if (clen > UNIX_BUFFER_SIZE) clen = UNIX_BUFFER_SIZE;
  memcpy (iobuf, &Byte (buf, Long_val (first)), clen);
  caml_release_runtime_system ();
  int rc = mbedtls_ssl_write (cctx, iobuf, clen);
  caml_acquire_runtime_system ();

  if (rc < 0) return C_mbedtls_rc_to_val (rc);
  else return (Val_long (rc));
}

CAMLprim value
ocaml_bytesrw_mbedtls_ssl_close_notify (value ctx)
{
  return C_mbedtls_rc_to_val
    (mbedtls_ssl_close_notify (C_mbedls_ssl_context_ptr_of_val (ctx)));
}
