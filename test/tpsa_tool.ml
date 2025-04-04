(*---------------------------------------------------------------------------
   Copyright (c) 2022 The tpsa programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf
let ( let* ) = Result.bind

let list_map_stop_on_error f l =
  let rec loop f acc = function
  | [] -> Ok (List.rev acc)
  | v :: vs -> match f v with Error _ as e -> e | Ok w -> loop f (w :: acc) vs
  in
  loop f [] l

let with_file file f =
  try match file with
  | "-" -> f file stdin
  | file ->
      In_channel.with_open_bin file @@ fun ic ->
      try f file ic with
      | Sys_error e -> Error (strf "%s: %s" file e)
  with Sys_error e -> Error e

let log_err s =
  Printf.eprintf "%s: %s\n%!" (Filename.basename Sys.executable_name) s

(* Hash *)

let output_hash oc (h, file) =
  let hex = Tpsa.Hex.of_octets (Tpsa.Hash.to_octets h) in
  Printf.fprintf oc "%s %s\n" hex file

let hash_file algo file =
  let hash_channel file ic =
    Tpsa.string_error @@
    let s = In_channel.input_all ic in
    let* hash = Tpsa.hash_compute algo (Bytes.unsafe_of_string s) in
    Ok (hash, file)
  in
  with_file file hash_channel

let compare_file_hash algo file h =
  let compare_hash_channel file ic =
    Tpsa.string_error @@
    let s = In_channel.input_all ic in
    match Tpsa.hash_compare algo (Bytes.unsafe_of_string s) h with
    | Ok () -> Ok true
    | Error e when Tpsa.Error.is_invalid_signature e -> Ok false
    | Error _ as e -> e
  in
  with_file file compare_hash_channel

let read_checksum_file file =
  let input_checksum file ic =
    let s = In_channel.input_all ic in
    let rec loop cs = function
    | [] -> Ok (List.rev cs)
    | l :: ls ->
        match String.split_on_char ' ' l with
        | [] -> assert false
        | [""] -> loop cs ls  (* Skip blank lines *)
        | c :: fname ->
            let c = String.trim c in
            match Tpsa.Hex.to_octets' c with
            | Error e -> Error (strf "%s: checksum %s: %s" file c e)
            | Ok c ->
                let fname = String.trim (String.concat " " fname) in
                let c' = Tpsa.Hash.of_octets c in
                loop ((c', fname) :: cs) ls
    in
    loop [] (String.split_on_char '\n' s)
  in
  with_file file input_checksum

let check_sums algo files =
  match list_map_stop_on_error read_checksum_file files with
  | Error e -> log_err e; Ok Cmdliner.Cmd.Exit.some_error
  | Ok csums ->
      let rec loop err = function
      | [] ->
          if err = 0 then Ok 0 else
          let s = if err > 1 then "s" else "" in
          (log_err (strf "Error: %d check%s errored" err s);
           Ok Cmdliner.Cmd.Exit.some_error;)
      | (h, file) :: csums ->
          match compare_file_hash algo file h with
          | Error e -> log_err e; loop (err + 1) csums
          | Ok true ->
              Printf.printf "%s: OK\n%!" file; loop err csums
          | Ok false ->
              Printf.printf "%s: FAILED\n%!" file; loop (err + 1) csums
      in
      loop 0 (List.concat csums)

let hash_files algo files =
  match list_map_stop_on_error (hash_file algo) files with
  | Error e -> log_err e; Ok Cmdliner.Cmd.Exit.some_error
  | Ok hs -> List.iter (output_hash stdout) hs; Ok 0

let hash algo files check =
  Tpsa.string_error @@
  let* () = Tpsa.crypto_init () in
  let files = match files with [] -> ["-"] | files -> files in
  if check then check_sums algo files else hash_files algo files

(* Key *)

let gen_curve25519_key_pair () =
  let usage = Tpsa.Key_usage.export in
  let type' = Tpsa.Key_type.(ecc_key_pair ecc_family_montgomery) in
  let bits = 255 in
  let att = Tpsa.Key_attributes.make ~type' ~bits ~usage () in
  let* kid = Tpsa.generate_key att in
  let* sec = Tpsa.export_key kid in
  let* pub = Tpsa.export_public_key kid in
  let* () = Tpsa.destroy_key kid in
  Ok (sec, pub)

let key () =
  Tpsa.string_error @@
  let* () = Tpsa.crypto_init () in
  let* sec, pub = gen_curve25519_key_pair () in
  let output_hex oc b = output_string oc (Tpsa.Hex.of_bytes b) in
  output_string stdout "secret: "; output_hex stdout sec;
  output_char stdout '\n';
  output_string stdout "public: "; output_hex stdout pub;
  Ok 0

(* Random *)

let random count hex =
  Tpsa.string_error @@
  let* () = Tpsa.crypto_init () in
  let count, infinite = match count with
  | None -> 1024, true | Some c -> c, false
  in
  let b = Bytes.create count in
  let gen () =
    let* () = Tpsa.generate_random b in
    if not hex
    then Ok (output_bytes stdout b)
    else Ok (output_string stdout (Tpsa.Hex.of_bytes b))
  in
  if not infinite then (let* () = gen () in Ok 0) else
  let rec loop () = match gen () with
  | Error _ as e -> e
  | Ok () -> loop ()
  in
  loop ()


open Cmdliner

let exits = Cmd.Exit.defaults
let common_man =
  [ `S Manpage.s_bugs;
    `P "This program is distributed with the Tpsa OCaml library. \
        See $(i,https://erratique.ch/software/tmbedtls) for contact \
        information."; ]

let hash_algos =
  ["md2", Tpsa.Algorithm.md2;
   "md4", Tpsa.Algorithm.md4;
   "md5", Tpsa.Algorithm.md4;
   "ripemd160", Tpsa.Algorithm.ripemd160;
   "sha-1", Tpsa.Algorithm.sha_1;
   "sha-224", Tpsa.Algorithm.sha_1;
   "sha-256", Tpsa.Algorithm.sha_256;
   "sha-384", Tpsa.Algorithm.sha_384;
   "sha-512", Tpsa.Algorithm.sha_512;
   "sha-512-224", Tpsa.Algorithm.sha_512_224;
   "sha-512-256", Tpsa.Algorithm.sha_512_256;
   "sha3-224", Tpsa.Algorithm.sha3_224;
   "sha3-256", Tpsa.Algorithm.sha3_256;
   "sha3-384", Tpsa.Algorithm.sha3_384;
   "sha3-512", Tpsa.Algorithm.sha3_384;
   "shake256-512", Tpsa.Algorithm.sha3_384;
   "sm3", Tpsa.Algorithm.sm3; ]

let hash_cmd =
  let doc = "Hash bytes" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) compute and check file hashes.";
    `Blocks common_man; ]
  in
  let algo =
    let algo = Arg.enum hash_algos and doc = Arg.doc_alts_enum hash_algos in
    let doc = strf "The hash algorithm. Must be one of %s." doc in
    Arg.(required & pos 0 (some algo) None & info [] ~doc ~docv:"HASH")
  in
  let files =
    let doc = "The files to hash, if unspecified or $(b,-) reads data \
               from $(b,stdin). Repeatable."
    in
    Arg.(value & pos_right 0 string [] & info [] ~doc ~docv:"FILE")
  in
  let check =
    let doc = "Read checksums from $(i,FILE)s in the format output \
               by and verify them." in
    Arg.(value & flag & info ["c"; "check"] ~doc)
  in
  Cmd.v (Cmd.info "hash" ~doc ~exits ~man)
    Term.(const hash $ algo $ files $ check)

let key_cmd =
  let doc = "Key management" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) manages keys.";
    `Blocks common_man; ]
  in
  Cmd.v (Cmd.info "key" ~doc ~exits ~man)
    Term.(const key $ const ())

let random_cmd =
  let doc = "Generate random bytes" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) outputs random bytes.";
    `Blocks common_man; ]
  in
  let count =
    let doc = "Number of bytes to generate. Infinite if unspecified." in
    Arg.(value & pos 0 (some int) None & info [] ~doc ~docv:"COUNT")
  in
  let hex =
    let doc = "Generate the bytes in US-ASCII hexdecimal" in
    Arg.(value & flag & info ["h"; "hex"] ~doc)
  in
  Cmd.v (Cmd.info "random" ~doc ~exits ~man)
    Term.(const random $ count $ hex)

let cmd =
  let doc = "PSA cryptography tool" in
  let sdocs = Manpage.s_common_options in
  let man = [
    `S Manpage.s_description;
    `P "$(mname) is a command line tool providing access to the \
        functionality of the PSA cryptography API.";
    `Blocks common_man; ]
  in
  Cmd.group
    (Cmd.info "psacrypto" ~version:"%%VERSION%%" ~doc ~sdocs ~exits ~man)
    [hash_cmd; key_cmd; random_cmd]

let main () = Cmd.eval_result' cmd
let () = if !Sys.interactive then () else exit (main ())
