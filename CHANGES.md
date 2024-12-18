
- Fix wrong bound checks in `Slice.{sub,make}[_or_eod]`. The functions
  now behave like `Bytes.sub` as far as indexing in allowed. Thanks
  to Adrián Montesinos González for the report and suggesting the fix (#4).
- `bytesrw.*` libraries are made to export `bytesrw`.

v0.1.0 2024-11-28 Zagreb
------------------------

First release.

Supported by a grant from the OCaml Software Foundation.
