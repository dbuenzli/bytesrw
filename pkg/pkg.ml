#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let blake3 = Conf.with_pkg "conf-libblake3"
let libmd = Conf.with_pkg "conf-libmd"
let xxhash = Conf.with_pkg "conf-xxhash"
let zstd = Conf.with_pkg "conf-zstd"
let zlib = Conf.with_pkg "conf-zlib"
let () =
  Pkg.describe "bytesrw" @@ fun c ->
  let blake3 = Conf.value c blake3 in
  let libmd = Conf.value c libmd in
  let xxhash = Conf.value c xxhash in
  let zlib = Conf.value c zlib in
  let zstd = Conf.value c zstd in
  Ok [ Pkg.mllib ~api:["Bytesrw"; "Bytesrw_utf"; "Bytesrw_hex"]
         "src/bytesrw.mllib";
       Pkg.mllib "src/unix/bytesrw_unix.mllib" ~dst_dir:"unix";
       Pkg.mllib ~cond:blake3 "src/blake3/bytesrw_blake3.mllib"
         ~dst_dir:"blake3";
       Pkg.clib ~cond:blake3 "src/blake3/libbytesrw_blake3_stubs.clib"
         ~lib_dst_dir:"blake3";
       Pkg.mllib ~cond:libmd "src/md/bytesrw_md.mllib"
         ~dst_dir:"md";
       Pkg.clib ~cond:libmd "src/md/libbytesrw_md_stubs.clib"
         ~lib_dst_dir:"md";
       Pkg.mllib "src/sysrandom/bytesrw_sysrandom.mllib"
         ~dst_dir:"sysrandom";
       Pkg.clib "src/sysrandom/libbytesrw_sysrandom_stubs.clib"
          ~lib_dst_dir:"sysrandom";
       Pkg.mllib ~cond:xxhash "src/xxhash/bytesrw_xxhash.mllib"
         ~dst_dir:"xxhash";
       Pkg.clib ~cond:xxhash "src/xxhash/libbytesrw_xxhash_stubs.clib"
         ~lib_dst_dir:"xxhash";
       Pkg.mllib ~cond:zlib "src/zlib/bytesrw_zlib.mllib" ~dst_dir:"zlib";
       Pkg.clib ~cond:zlib "src/zlib/libbytesrw_zlib_stubs.clib"
         ~lib_dst_dir:"zlib";
       Pkg.mllib ~cond:zstd "src/zstd/bytesrw_zstd.mllib" ~dst_dir:"zstd";
       Pkg.clib ~cond:zstd "src/zstd/libbytesrw_zstd_stubs.clib"
         ~lib_dst_dir:"zstd";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/cookbook.mld" ~dst:"odoc-pages/cookbook.mld";
       Pkg.doc "doc/notes.mld" ~dst:"odoc-pages/notes.mld";
       Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";]
