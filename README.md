Bytesrw – Composable byte stream readers and writers for OCaml
==============================================================

Bytesrw extends the OCaml `Bytes` module with composable, memory
efficient, byte stream readers and writers compatible with effect
based concurrency.

Except for byte slice life-times, these abstractions intentionally
separate away ressource management and the specifics of reading and
writing bytes.

Bytesrw distributed under the ISC license. It has no dependencies.

Optional support for compressed bytes and hashes depends on the C [`zlib`],
[`libzstd`], [`xxhash`] and [`blake3`] libraries.

[`zlib`]: https://zlib.net
[`libzstd`]: http://zstd.net
[`xxhash`]: https://xxhash.com/
[`blake3`]: https://blake3.io

Homepage: <https://erratique.ch/software/bytesrw/>

## Installation

Bytesrw can be installed with `opam`

    opam install bytesrw 
    opam install bytesrw \
                 conf-zlib conf-zstd \ # compression support
                 conf-xxhash           # hashing support
                 # conf-blake3 need to create one in opam

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online] or via `odig doc bytesrw`.

Questions are welcome but better asked on the [OCaml forum] than on the
issue tracker. 

[online]: https://erratique.ch/software/bytesrw/doc
[OCaml forum]: https://discuss.ocaml.org/

## Examples

A few examples can be found in the [test](test/) directory. 
