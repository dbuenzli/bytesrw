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
[`libzstd`], [`xxhash`] [`libmd`] and [`blake3`] libraries.

[`blake3`]: https://blake3.io
[`libzstd`]: http://zstd.net
[`libmd`]: https://www.hadrons.org/software/libmd/
[`xxhash`]: https://xxhash.com/
[`zlib`]: https://zlib.net

Homepage: <https://erratique.ch/software/bytesrw/>

## Installation

Bytesrw can be installed with `opam`

    opam install bytesrw 
    opam install bytesrw \
               \ # compression support
                 conf-zlib conf-zstd \
               \ # hashing support
                 conf-xxhash conf-libblake3 conf-libmd

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
