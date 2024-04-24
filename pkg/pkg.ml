#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let zstd = Conf.with_pkg "conf-zstd"
let zlib = Conf.with_pkg "conf-zlib"
let () =
  Pkg.describe "bytesrw" @@ fun c ->
  let zstd = Conf.value c zstd in
  let zlib = Conf.value c zlib in
  Ok [ Pkg.mllib ~api:["Bytesrw"] "src/bytesrw.mllib";
       Pkg.mllib "src/kit/bytesrw.mllib";
       Pkg.mllib ~cond:zlib "src/zlib/bytesrw_zlib.mllib" ~dst_dir:"zlib";
       Pkg.clib ~cond:zlib "src/zlib/libbytesrw_zlib_stubs.clib"
         ~lib_dst_dir:"zlib";
       Pkg.mllib ~cond:zstd "src/zstd/bytesrw_zstd.mllib" ~dst_dir:"zstd";
       Pkg.clib ~cond:zstd "src/zstd/libbytesrw_zstd_stubs.clib"
         ~lib_dst_dir:"zstd";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
       Pkg.doc "doc/conventions.mld" ~dst:"odoc-pages/convention.mld"]
