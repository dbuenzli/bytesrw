open B0_kit.V000

(* Library names *)

let bytesrw = B0_ocaml.libname "bytesrw"
let bytesrw_blake3 = B0_ocaml.libname "bytesrw.blake3"
let bytesrw_crypto = B0_ocaml.libname "bytesrw.crypto"
let bytesrw_md = B0_ocaml.libname "bytesrw.md"
let bytesrw_sysrandom = B0_ocaml.libname "bytesrw.sysrandom"
let bytesrw_tls = B0_ocaml.libname "bytesrw.tls"
let bytesrw_unix = B0_ocaml.libname "bytesrw.unix"
let bytesrw_xxhash = B0_ocaml.libname "bytesrw.xxhash"
let bytesrw_zlib = B0_ocaml.libname "bytesrw.zlib"
let bytesrw_zstd = B0_ocaml.libname "bytesrw.zstd"

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let bytesrw_lib =
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib bytesrw ~srcs

let bytesrw_blake3_lib =
  let doc = "BLAKE3 hashes" in
  let srcs = [ `Dir ~/"src/blake3" ] in
  let c_requires = Cmd.arg "-lblake3" in
  let requires = [bytesrw] and exports = [bytesrw] in
  B0_ocaml.lib bytesrw_blake3 ~srcs ~requires ~exports ~c_requires ~doc

let bytesrw_crypto_lib =
  let doc = "Cryptographic primitives" in
  let srcs = [ `Dir ~/"src/crypto" ] in
  let c_requires = Cmd.arg "-ltfpsacrypto" in
  let requires = [bytesrw] and exports = [bytesrw] in
  B0_ocaml.lib bytesrw_crypto ~srcs ~requires ~exports ~c_requires ~doc

let bytesrw_tls_lib =
  let doc = "TLS streams" in
  let srcs = [ `Dir ~/"src/tls" ] in
  let c_requires =
    Cmd.(arg "-lmbedx509" % "-lmbedtls" %
         (* FIXME only on macOS *)
         "-framework" % "Security" % "-framework" % "CoreFoundation")
  in
  let requires = [bytesrw; bytesrw_crypto; unix] in
  let exports = [bytesrw; bytesrw_crypto; unix] in
  B0_ocaml.lib bytesrw_tls ~srcs ~requires ~exports ~c_requires ~doc

let bytesrw_md_lib =
  let doc = "SHA{1,2} hashes" in
  let srcs = [ `Dir ~/"src/md" ] in
  let c_requires = Cmd.arg "-lmd" in
  let requires = [bytesrw] and exports = [bytesrw] in
  B0_ocaml.lib bytesrw_md ~srcs ~requires ~exports ~c_requires ~doc

let bytesrw_sysrandom_lib =
  let doc = "Cryptographically secure pseudorandom byte streams" in
  let srcs = [ `Dir ~/"src/sysrandom" ] in
  let requires = [bytesrw] and exports = [bytesrw] in
  B0_ocaml.lib bytesrw_sysrandom ~srcs ~requires ~exports ~doc

let bytesrw_unix_lib =
  let srcs = [ `Dir ~/"src/unix" ] in
  let requires = [bytesrw; unix] and exports = [bytesrw] in
  B0_ocaml.lib bytesrw_unix ~srcs ~requires ~exports

let bytesrw_xxhash_lib =
  let doc = "XXH hashes" in
  let srcs = [ `Dir ~/"src/xxhash" ] in
  let c_requires = Cmd.arg "-lxxhash" in
  let requires = [bytesrw] and exports = [bytesrw]in
  B0_ocaml.lib bytesrw_xxhash ~srcs ~requires ~exports ~c_requires ~doc

let bytesrw_zlib_lib =
  let doc = "deflate, zlib and gzip streams" in
  let srcs = [ `Dir ~/"src/zlib" ] in
  let c_requires = Cmd.arg "-lz" in
  let requires = [bytesrw] and exports = [bytesrw] in
  B0_ocaml.lib bytesrw_zlib ~srcs ~requires ~exports ~c_requires ~doc

let bytesrw_zstd_lib =
  let doc = "zstd streams" in
  let srcs = [ `Dir ~/"src/zstd" ] in
  let c_requires = Cmd.arg "-lzstd" in
  let requires = [bytesrw] and exports = [bytesrw] in
  B0_ocaml.lib bytesrw_zstd ~srcs ~requires ~exports ~c_requires ~doc

(* Tests *)

let test ?(requires = []) =
  B0_ocaml.test ~requires:(b0_std :: bytesrw :: requires)

let utf8codec = test ~/"test/utf8codec.ml" ~long:true

let test_quickstart =
  test ~/"test/quickstart.ml" ~requires:[bytesrw_zstd] ~run:false

let test_cookbook =
  let requires = [bytesrw_zstd; bytesrw_blake3] in
  test ~/"test/cookbook.ml" ~requires ~run:false

let test_bytesrw = test ~/"test/test_bytesrw.ml" ~requires:[]
let test_utf = test ~/"test/test_utf.ml"
let test_blake3 = test ~/"test/test_blake3.ml" ~requires:[bytesrw_blake3]
let test_psa = test ~/"test/test_psa.ml" ~requires:[bytesrw_crypto]
let test_crypto = test ~/"test/test_crypto.ml" ~requires:[bytesrw_crypto]
let test_md = test ~/"test/test_md.ml" ~requires:[bytesrw_md]
let test_sysrandom =
  test ~/"test/test_sysrandom.ml" ~requires:[bytesrw_sysrandom]

let test_xxhash = test ~/"test/test_xxhash.ml" ~requires:[bytesrw_xxhash]
let test_zlib = test ~/"test/test_zlib.ml" ~requires:[bytesrw_zlib]
let test_zstd = test ~/"test/test_zstd.ml" ~requires:[bytesrw_zstd]
let test_tls = test ~/"test/test_tls.ml" ~requires:[bytesrw_tls; threads]

let tool_requires = [cmdliner; unix; bytesrw_unix]

let blake3tap =
  let doc = "Hash stdin with blake3" in
  let requires = bytesrw_blake3 :: tool_requires in
  test ~/"test/blake3tap.ml" ~run:false ~requires ~doc

let xxh3tap =
  let doc = "Hash stdin with xxh3" in
  let requires = bytesrw_xxhash :: tool_requires in
  test ~/"test/xxh3tap.ml" ~run:false ~requires ~doc

let gziptrip =
  let doc = "Gzip (De)compression from stdin to stdout" in
  let requires = bytesrw_zlib :: tool_requires in
  test ~/"test/gziptrip.ml" ~run:false ~requires ~doc

let zstdtrip =
  let doc = "Zstd (De)compression from stdin to stdout" in
  let requires = bytesrw_zstd :: tool_requires in
  test ~/"test/zstdtrip.ml" ~run:false ~requires ~doc

let webfetch =
  let doc = "Fetch HTTP(S) URLs" in
  let requires = bytesrw_tls :: b0_std :: tool_requires in
  test ~/"test/webfetch.ml" ~run:false ~requires ~doc

let webserve =
  let doc = "Echo HTTPS requests" in
  let requires = bytesrw_tls :: threads :: b0_std :: tool_requires in
  test ~/"test/webserve.ml" ~run:false ~requires ~doc

let min_tls =
  let doc = "Minimal TLS example" in
  let requires = bytesrw_tls :: b0_std :: tool_requires in
  test ~/"test/min_tls.ml" ~run:false ~requires ~doc

let certown =
  let doc = "Basic X509 certificate munging for dev" in
  let requires = bytesrw_tls :: b0_std :: tool_requires in
  let srcs = [ `File ~/"test/certown.ml" ] in
  B0_ocaml.exe "certown" ~srcs ~requires ~doc

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
     ["bytes"; "entropy"; "streaming"; "zstd"; "zlib"; "gzip"; "deflate";
      "random"; "csprng"; "sha1"; "sha2"; "compression"; "hashing";
      "utf"; "xxhash"; "blake3"; "psa"; "tls";
      "cryptography"; "sha3"; "org:erratique"; ]
   |> ~~ B0_opam.build
     {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                 "--with-cmdliner" "%{cmdliner:installed}%"
                 "--with-b0" "%{b0:installed}%"
                 "--with-conf-libblake3" "%{conf-libblake3:installed}%"
                 "--with-conf-mbedtls" "%{conf-mbedtls:installed}%"
                 "--with-conf-libmd" "%{conf-libmd:installed}%"
                 "--with-conf-xxhash" "%{conf-xxhash:installed}%"
                 "--with-conf-zlib" "%{conf-zlib:installed}%"
                 "--with-conf-zstd" "%{conf-zstd:installed}%"]
          ["cmdliner" "install" "tool-support"
          "--update-opam-install=%{_:name}%.install"
          "_build/test/certown.native:certown" {ocaml:native}
          "_build/test/certown.byte:certown" {!ocaml:native}
          "_build/cmdliner-install"] {cmdliner:installed &
                                      b0:installed &
                                      conf-mbedtls:installed} ]|}
   |> ~~ B0_opam.depopts ["conf-mbedtls", "";
                          "conf-xxhash", "";
                          "conf-zlib", "";
                          "conf-zstd", "";
                          "conf-libmd", "";
                          "conf-libblake3", "";
                          "cmdliner", "";
                          "b0", "";
                         ]
   |> ~~ B0_opam.conflicts [ "conf-zstd", {|< "1.3.8"|}; (* should be 1.4 *)
                             "cmdliner", {|< "2.0.0"|};
                             "b0", {|< "0.0.7"|};
                           ]
   |> ~~ B0_opam.depends
     [ "ocaml", {|>= "4.14.0"|};
       "ocamlfind", {|build|};
       "ocamlbuild", {|build|};
       "topkg", {|build & >= "1.1.1"|};
       "conf-pkg-config", {|build|}; ]
   |> B0_meta.tag B0_opam.tag
 in
 B0_pack.make "default" ~doc:"The bytesrw package" ~meta ~locked:true @@
 B0_unit.list ()
