#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let xxhash = Conf.with_pkg "conf-xxhash"
let zstd = Conf.with_pkg "conf-zstd"
let zlib = Conf.with_pkg "conf-zlib"
let () =
  Pkg.describe "bytesrw" @@ fun c ->
  let xxhash = Conf.value c xxhash in
  let zlib = Conf.value c zlib in
  let zstd = Conf.value c zstd in
  Ok [ Pkg.mllib ~api:["Bytesrw"] "src/bytesrw.mllib";
       Pkg.mllib "src/kit/bytesrw_kit.mllib" ~dst_dir:"kit";
       Pkg.mllib "src/unix/bytesrw_unix.mllib" ~dst_dir:"unix";
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
       Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";]
