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
