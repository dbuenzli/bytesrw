open B0_kit.V000

(* Library names *)

let bytesrw = B0_ocaml.libname "bytesrw"

(* Libraries *)

let bytesrs_lib =
  let srcs = [ `Dir ~/"src" ] in
  B0_ocaml.lib bytesrw ~srcs

(* Tests *)

let test ?(requires = []) src =
  let srcs = [ `File src ] in
  let requires = bytesrw :: requires in
  let meta = B0_meta.empty |> B0_meta.tag B0_meta.test in
  let name = Fpath.basename ~strip_ext:true src in
  B0_ocaml.exe name ~srcs ~requires ~meta

let utf8codec = test ~/"test/utf8codec.ml"

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
   |> ~~ B0_meta.description_tags ["bytes"; "streaming"; "org:erratique"; ]
   |> ~~ B0_opam.build
     {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
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
