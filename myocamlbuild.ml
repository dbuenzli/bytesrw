open Ocamlbuild_plugin
open Command

(* Generic pkg-config(1) support. *)

let pkg_config_exists package =
  Sys.command ("pkg-config --exists " ^ package) = 0

let lib_with_clib ~lib ~clib ~has_lib ~src_dir ~stublib =
  let strf = Printf.sprintf in
  let windows = !Ocamlbuild_plugin.Options.ext_lib = "lib" in
  let pkg_config flags package =
    let cmd tmp =
      let pkg_config =
        if not windows then A "pkg-config" else
        S [A "pkg-config"; A "--msvc-syntax"]
      in
      Command.execute ~quiet:true &
      Cmd( S [ pkg_config; A ("--" ^ flags); A package; Sh ">"; A tmp]);
      List.map (fun arg -> A arg) (string_list_of_file tmp)
    in
    with_temp_file "pkgconfig" "pkg-config" cmd
  in
  let ar s = match !Ocamlbuild_plugin.Options.ext_lib with
  | "" -> s ^ ".a" | x -> s ^ "." ^ x
  in
  let make_opt o arg = S [ A o; arg ] in
  let ccopts = List.map (make_opt "-ccopt") in
  let cclibs = List.map (make_opt "-cclib") in
  let dllibs = List.map (make_opt "-dllib") in
  let use_lib = strf "use_%s" lib in
  let use_clib = strf "use_%s" clib in
  let record_stub_lib = strf "record_%s" stublib in
  let link_stub_archive = strf "link_%s_archive" stublib in
  let stub_ar = ar (strf "%s/lib%s" src_dir stublib) in
  let static_stub_l =
    if windows then A (strf "lib%s.lib" stublib) else A (strf "-l%s" stublib)
  in
  let dynamic_stub_l =
    if windows then A (strf "dll%s.dll" stublib) else static_stub_l
  in
  let clib_l = pkg_config "libs-only-l" clib in
  let clib_L =
    let dashldify = function
    | A l when windows -> A (String.subst "/libpath:" "-L" l)
    | arg -> arg
    in
    List.map dashldify (pkg_config "libs-only-L" clib)
  in
  let clib_cflags = ccopts @@ (A has_lib) :: pkg_config "cflags" clib in
  let clib_cclibs = cclibs @@ static_stub_l :: clib_l in
  let clib_ccopts = ccopts @@ clib_L in
  begin
    dep [record_stub_lib] [stub_ar];

    flag ["c"; "compile"; use_clib] (S clib_cflags);

    flag ["c"; "ocamlmklib"; use_clib] (S (clib_L @ clib_l));

    flag ["link"; "ocaml"; "library"; "byte"; record_stub_lib]
      (S (dllibs [dynamic_stub_l] @ clib_ccopts @ clib_cclibs));

    flag ["link"; "ocaml"; "library"; "native"; record_stub_lib]
      (S (clib_ccopts @ clib_cclibs));

    flag_and_dep ["link"; "ocaml"; link_stub_archive] (P stub_ar);

    flag ["link"; "ocaml"; "library"; "shared"; link_stub_archive]
      (S (clib_ccopts @ clib_cclibs));

    ocaml_lib ~tag_name:use_lib ~dir:src_dir (strf "%s/%s" src_dir lib)
  end

let lib s = match !Ocamlbuild_plugin.Options.ext_lib with
| "" -> s ^ ".a"
| x -> s ^ "." ^ x

let () =
  dispatch begin function
  | After_rules ->
      if pkg_config_exists "libblake3" then
        lib_with_clib
          ~lib:"bytesrw_blake3" ~clib:"libblake3" ~has_lib:"-DHAS_BLAKE3"
          ~src_dir:"src/blake3" ~stublib:"bytesrw_blake3_stubs";
      if pkg_config_exists "mbedcrypto" then
        lib_with_clib
          ~lib:"bytesrw_crypto" ~clib:"mbedcrypto" ~has_lib:"-DHAS_PSA_CRYPTO"
          ~src_dir:"src/crypto" ~stublib:"bytesrw_crypto_stubs";
      if pkg_config_exists "libmd" then
        lib_with_clib
          ~lib:"bytesrw_md" ~clib:"libmd" ~has_lib:"-DHAS_LIBMD"
          ~src_dir:"src/md" ~stublib:"bytesrw_md_stubs";
      if pkg_config_exists "libxxhash" then
        lib_with_clib
          ~lib:"bytesrw_xxhash" ~clib:"libxxhash" ~has_lib:"-DHAS_XXHASH"
          ~src_dir:"src/xxhash" ~stublib:"bytesrw_xxhash_stubs";
      if pkg_config_exists "zlib" then
        lib_with_clib
          ~lib:"bytesrw_zlib" ~clib:"zlib" ~has_lib:"-DHAS_ZLIB"
          ~src_dir:"src/zlib" ~stublib:"bytesrw_zlib_stubs";
      if pkg_config_exists "libzstd" then
        lib_with_clib
          ~lib:"bytesrw_zstd" ~clib:"libzstd" ~has_lib:"-DHAS_ZSTD"
          ~src_dir:"src/zstd" ~stublib:"bytesrw_zstd_stubs";

      (* bytesrw.sysrandom *)

      dep ["record_bytesrw_sysrandom_stubs"]
        [lib "src/sysrandom/libbytesrw_sysrandom_stubs"];

      flag_and_dep
        ["link"; "ocaml"; "link_bytesrw_sysrandom_stubs"]
        (P (lib "src/sysrandom/libbytesrw_sysrandom_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_bytesrw_sysrandom_stubs"]
        (S ([A "-dllib"; A "-lbytesrw_sysrandom_stubs"]));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_bytesrw_sysrandom_stubs"]
        (S ([A "-cclib"; A "-lbytesrw_sysrandom_stubs"]));

  | _ -> ()
  end
