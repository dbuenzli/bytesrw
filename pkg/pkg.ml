#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "bytesrw" @@ fun c ->
  Ok [ Pkg.mllib "src/bytesrw.mllib";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld" ]
