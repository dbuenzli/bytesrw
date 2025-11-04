
- `Bytesrw_sysrandom`: fix headers for musl libc (alpine.) (#7)

v0.3.0 2025-11-04 Zagreb
------------------------

- Add the `Bytesrw_sysrandom` module for operating system provided
  cryptographically secure pseudorandom byte streams and an entropy
  primitive.
  
- Add the optional `Bytesrw_crypto.Psa` module for low-level
  cryptographic operations on byte slices. These are thin and safe
  bindings to the PSA Crypto API specification currently provided by
  the TF-PSA-Crypto C library distributed with Mbed TLS.
  
- Add the optional `Bytesrw_crypto` module for higher-level
  cryptographic operations implemented over `Bytesrw_crypto.Psa`.
  Mostly hashing for now. Use this if you need SHA-3 hashes.

- Add the optional `Bytesrw_tls` a module for TLS encrypted streams
  and the needed X.509 certificate management (including system
  lookups for trusted CAs). The backend is provided by the Mbed TLS C
  library.

- Add `Bytesrw_unix.bytes_writer_of_socket_fd` which shutdowns the
  fd in the send direction when the end of stream is written.

- Add `Bytes.Slice.last`.

- Review `Int_val` vs `Long_val` in C bindings. Affects
  `Bytesrw_blake3`, `Bytesrw_md`, `Bytesrw_xxh`, `Bytesrw_zlib`,
  `Bytesrw_zstd`

v0.2.0 2025-07-25 Zagreb
------------------------

- Fix `Bytesrw_xxhash.Xxh64.{to_hex,pp}`. Leading zeros
  were not being printed (#5).
- Change unuseful signature of `Slice.break`: do not return 
  `None` if any of `Slice.take` or `Slice.drop` does. Simply
  return the result of both operations.
- Fix wrong bound checks in `Slice.{sub,make}[_or_eod]`. The functions
  now behave like `Bytes.sub` as far as indexing is allowed. Thanks
  to Adrián Montesinos González for the report and suggesting the fix (#4).
- `bytesrw.*` libraries are made to export `bytesrw`.

v0.1.0 2024-11-28 Zagreb
------------------------

First release.

Supported by a grant from the OCaml Software Foundation.
