open B0_kit.V000

(* Library names *)

let bytesrw = B0_ocaml.libname "bytesrw"
let bytesrw_kit = B0_ocaml.libname "bytesrw.kit"

let bytesrw_blake3 = B0_ocaml.libname "bytesrw.blake3"
let bytesrw_md = B0_ocaml.libname "bytesrw.md"
let bytesrw_unix = B0_ocaml.libname "bytesrw.unix"
let bytesrw_xxhash = B0_ocaml.libname "bytesrw.xxhash"
let bytesrw_zlib = B0_ocaml.libname "bytesrw.zlib"
let bytesrw_zstd = B0_ocaml.libname "bytesrw.zstd"

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let bytesrw_lib =
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib bytesrw ~srcs

let bytesrw_kit_lib =
  let srcs = [ `Dir ~/"src/kit" ] in
  let requires = [bytesrw] in
  B0_ocaml.lib bytesrw_kit ~srcs ~requires

let bytesrw_blake3_lib =
  let doc = "BLAKE3 hashes" in
  let srcs = [ `Dir ~/"src/blake3" ] in
  let c_requires = Cmd.arg "-lblake3" in
  let requires = [bytesrw] in
  B0_ocaml.lib bytesrw_blake3 ~srcs ~requires ~c_requires ~doc

let bytesrw_md_lib =
  let doc = "SHA{1,2} hashes" in
  let srcs = [ `Dir ~/"src/md" ] in
  let c_requires = Cmd.arg "-lmd" in
  let requires = [bytesrw] in
  B0_ocaml.lib bytesrw_md ~srcs ~requires ~c_requires ~doc

let bytesrw_unix_lib =
  let srcs = [ `Dir ~/"src/unix" ] in
  let requires = [bytesrw; unix] in
  B0_ocaml.lib bytesrw_unix ~srcs ~requires

let bytesrw_xxhash_lib =
  let doc = "XXH hashes" in
  let srcs = [ `Dir ~/"src/xxhash" ] in
  let c_requires = Cmd.arg "-lxxhash" in
  let requires = [bytesrw] in
  B0_ocaml.lib bytesrw_xxhash ~srcs ~requires ~c_requires ~doc

let bytesrw_zlib_lib =
  let doc = "deflate, zlib and gzip compressed bytes" in
  let srcs = [ `Dir ~/"src/zlib" ] in
  let c_requires = Cmd.arg "-lz" in
  let requires = [bytesrw] in
  B0_ocaml.lib bytesrw_zlib ~srcs ~requires ~c_requires ~doc

let bytesrw_zstd_lib =
  let doc = "zstd compressed bytes" in
  let srcs = [ `Dir ~/"src/zstd" ] in
  let c_requires = Cmd.arg "-lzstd" in
  let requires = [bytesrw] in
  B0_ocaml.lib bytesrw_zstd ~srcs ~requires ~c_requires ~doc

(* Tests *)

let test ?run:(do_run = true) ?(requires = []) src =
  let srcs = [ `File src ] in
  let requires = bytesrw :: requires in
  let requires = if do_run then b0_std :: requires else requires in
  let meta = B0_meta.(empty |> tag test |> add run do_run) in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta

let utf8codec = test ~/"test/utf8codec.ml"

let test_examples = test ~requires:[bytesrw_zstd] ~/"test/examples.ml"
let test_bytesrw = test ~requires:[] ~/"test/test_bytesrw.ml"
let test_utf = test ~requires:[bytesrw_kit] ~/"test/test_utf.ml"
let test_blake3 = test ~requires:[bytesrw_blake3] ~/"test/test_blake3.ml"
let test_blake3 = test ~requires:[bytesrw_md] ~/"test/test_md.ml"
let test_xxhash = test ~requires:[bytesrw_xxhash] ~/"test/test_xxhash.ml"
let test_zlib = test ~requires:[bytesrw_zlib] ~/"test/test_zlib.ml"
let test_zstd = test ~requires:[bytesrw_zstd] ~/"test/test_zstd.ml"

let trip_requires = [cmdliner; unix; bytesrw_unix]

let tool_test = test ~run:false
let blake3tap =
  tool_test ~requires:(bytesrw_blake3 :: trip_requires) ~/"test/blake3tap.ml"

let xxh3tap =
  tool_test ~requires:(bytesrw_xxhash :: trip_requires) ~/"test/xxh3tap.ml"

let gziptrip =
  tool_test ~requires:(bytesrw_zlib :: trip_requires) ~/"test/gziptrip.ml"

let zstdtrip =
  tool_test ~requires:(bytesrw_zstd :: trip_requires) ~/"test/zstdtrip.ml"

(* Packs *)

let default =
 let meta =
   B0_meta.empty
   |> ~~ B0_meta.authors ["The bytesrw programmers"]
   |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
   |> ~~ B0_meta.homepage "https://erratique.ch/software/bytesrw"
   |> ~~ B0_meta.online_doc "https://erratique.ch/software/bytesrw/doc"
   |> ~~ B0_meta.licenses ["ISC"]
   |> ~~ B0_meta.repo "git+https://erratique.ch/repos/bytesrw.git"
   |> ~~ B0_meta.issues "https://github.com/dbuenzli/bytesrw/issues"
   |> ~~ B0_meta.description_tags
     ["bytes"; "streaming"; "zstd"; "zlib"; "gzip"; "deflate";
      "sha1"; "sha2"; "compression"; "hashing";
      "utf"; "xxhash"; "blake3"; "base64"; "org:erratique"; ]
   |> ~~ B0_opam.build
     {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                 "--with-conf-libblake3" "%{conf-libblake3:installed}%"
                 "--with-conf-libmd" "%{conf-libmd:installed}%"
                 "--with-conf-xxhash" "%{conf-xxhash:installed}%"
                 "--with-conf-zlib" "%{conf-zlib:installed}%"
                 "--with-conf-zstd" "%{conf-zstd:installed}%"]]|}
   |> ~~ B0_opam.depopts ["conf-xxhash", "";
                          "conf-zlib", "";
                          "conf-zstd", ""]
   |> ~~ B0_opam.conflicts [ "conf-zstd", {|< "1.3.8"|}] (* should be 1.4 *)
   |> ~~ B0_opam.depends
     [ "ocaml", {|>= "4.14.0"|};
       "ocamlfind", {|build|};
       "ocamlbuild", {|build|};
       "topkg", {|build & >= "1.0.3"|};
     ]
   |> B0_meta.tag B0_opam.tag
 in
 B0_pack.make "default" ~doc:"The bytesrw package" ~meta ~locked:true @@
 B0_unit.list ()
