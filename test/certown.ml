(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax

let appdir = "certown"
let get_data_dir ~data_dir = match data_dir with
| Some dir -> Ok dir
| None ->
    let* data_dir = Os.Dir.data () in
    Ok (Fpath.(data_dir / appdir))

let data_dir_ca_cert_prefix ~data_dir = Fpath.(data_dir / "ca")
let cert_files_of_prefix ~prefix =
  Fpath.(prefix + ".cert.pem"), Fpath.(prefix + ".key.pem")

let write_cert (cert, private_key) ~force ~prefix =
  let certfile, keyfile = cert_files_of_prefix ~prefix in
  let* () = (* clear paths *)
    if force then
      let* _existed = Os.File.delete certfile in
      let* _existed = Os.File.delete keyfile in
      Ok ()
    else
    let* cexists = Os.File.exists certfile in
    let* kexists = Os.File.exists keyfile in
    let add_if b v vs = if b then v :: vs else vs in
    let exists = add_if cexists certfile @@ add_if kexists keyfile [] in
    if exists = [] then Ok () else
    let files = Fmt.(list Fpath.pp ~sep:sp) in
    Fmt.error "@[<v>@[Certificate already exists: %a@]@,Use %a to overwrite.@]"
      files exists Fmt.code "--force"
  in
  let cert_pem = Bytesrw_tls.X509_certchain.to_pem cert in
  let* () = Os.File.write ~force ~make_path:true certfile cert_pem in
  let* () =
      if Sys.win32 then Ok () else
      try Ok (ignore (Unix.umask 0o077)) with
      | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
  in
  let* () =
    let keyfile = Fpath.to_string keyfile in
    Bytesrw_tls.X509_certchain.Private_key.write_pem_file keyfile private_key
  in
  Ok ()

let self_signed ~is_ca ~name ~force ~prefix =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* own_cert =
    Bytesrw_tls.X509_certchain.self_signed ~is_ca ~name ()
  in
  let* () = write_cert own_cert ~force ~prefix in
  Ok 0

let get_ca ~data_dir =
  let err_miss p =
    Fmt.error "@[<v>%a: No such file@,Did you run %a?@]" Fpath.pp p
      Fmt.code "certown ca create"
  in
  let ca_prefix = data_dir_ca_cert_prefix ~data_dir in
  let ca_certfile, ca_keyfile = cert_files_of_prefix ~prefix:ca_prefix in
  let* cert_exists = Os.File.exists ca_certfile in
  let* key_exists = Os.File.exists ca_keyfile in
  if not cert_exists then err_miss ca_certfile else
  if not key_exists then err_miss ca_keyfile else
  Ok (ca_certfile, ca_keyfile)

let ca_create ~data_dir ~name ~force =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data_dir = get_data_dir ~data_dir in
  let* () = match get_ca ~data_dir with
  | Error _ -> Ok ()
  | Ok _ ->
      if force then Ok () else
      Fmt.error "CA already exists use %a to regenerate" Fmt.code "--force"
  in
  let* name = match name with
  | Some n -> Ok n
  | None ->
      (* XXX unicode ?! *)
      try Ok (Unix.getlogin ()) with
      | Unix.Unix_error (e, _, _) -> Error (Unix.error_message e)
  in
  let prefix = data_dir_ca_cert_prefix ~data_dir in
  Ok (self_signed ~is_ca:true ~name ~force ~prefix)

let ca_cert_file ~data_dir =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data_dir = get_data_dir ~data_dir in
  let* ca_certfile, _ca_keyfile = get_ca ~data_dir in
  Fmt.pr "%a@." Fpath.pp ca_certfile;
  Ok 0

let ca_delete ~data_dir =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data_dir = get_data_dir ~data_dir in
  let prefix = data_dir_ca_cert_prefix ~data_dir in
  let certfile, keyfile = cert_files_of_prefix ~prefix in
  let* _existed = Os.File.delete certfile in
  let* _existed = Os.File.delete keyfile in
  Ok 0

let ca_install ~data_dir ~dry_run ~force =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let err_did_you_sudo e =
    Fmt.str "@[<v>%a@,Did you invoke with %a?@]" Fmt.lines e Fmt.code "sudo"
  in
  let* data_dir = get_data_dir ~data_dir in
  let* ca_certfile, ca_keyfile = get_ca ~data_dir in
  let* () = match Os.name () with
  | Darwin _ ->
      (* Note we tried to add it to the login keychain but it seems
         browsers do not see the addition. *)
      let keychain = Fpath.v "/Library/Keychains/System.keychain" in
      let cmd =
        Cmd.(tool "security" % "add-trusted-cert" % "-d" % "-r" %
             "trustRoot" % "-k" %% path keychain %% path ca_certfile)
      in
      if dry_run
      then (Fmt.pr "%a@." Cmd.pp_shell cmd; Ok ())
      else Result.map_error err_did_you_sudo (Os.Cmd.run cmd)
  | Linux _ ->
      (* This will likely not work on all distributions :-( *)
      let root_ca_dir = Fpath.v "/usr/local/share/ca-certificates" in
      let dst = Fpath.(root_ca_dir / "certown-cert.crt") in
      let cmd = Cmd.(tool "update-ca-certificates") in
      if dry_run then begin
        Fmt.pr "cp %a %a@." Fpath.pp ca_certfile Fpath.pp dst;
        Fmt.pr "%a@." Cmd.pp_shell cmd;
        Ok ()
      end else begin
        Result.map_error err_did_you_sudo @@
        let* () = Os.File.copy ~force ~make_path:false ca_certfile ~dst in
        Os.Cmd.run cmd
      end
  | os ->
      Fmt.error "%a: don't know how to install CA certificates yet"
        Os.Name.pp_id os
  in
  Ok 0

let ca_signed ~data_dir ~name ~prefix ~only_cert ~force =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data_dir = get_data_dir ~data_dir in
  let* ca_certfile, ca_keyfile = get_ca ~data_dir in
  let* ca_cert =
    Bytesrw_tls.X509_certchain.read_pem_file (Fpath.to_string ca_certfile)
  in
  let* ca_key =
    Bytesrw_tls.X509_certchain.Private_key.read_pem_file
      (Fpath.to_string ca_keyfile)
  in
  let ca = ca_cert, ca_key in
  let* own = Bytesrw_tls.X509_certchain.ca_signed ~ca ~name () in
  let* () = write_cert own ~force ~prefix in
  Ok 0

let ca_info ~data_dir ~pem =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data_dir = get_data_dir ~data_dir in
  let* () = match get_ca ~data_dir with
  | Ok (ca_certfile, _ca_keyfile) ->
      let file = Fpath.to_string ca_certfile in
      let* cert = Bytesrw_tls.X509_certchain.read_pem_file file in
      let pp =
        if pem
        then Bytesrw_tls.X509_certchain.pp_pem
        else Bytesrw_tls.X509_certchain.pp
      in
      Ok (Fmt.pr "@[%a@]@." pp cert)
  | Error _ ->
      Fmt.error
        "@[<v>The CA was not found in %a@,Run %a to create it."
        Fpath.pp data_dir Fmt.code "certown ca create"
  in
  Ok 0

let info ~data_dir ~certs ~ca ~system_ca ~pem =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data_dir = get_data_dir ~data_dir in
  let certs =
    if not ca then certs else
    let ca_prefix = data_dir_ca_cert_prefix ~data_dir in
    let ca_certfile, _ = cert_files_of_prefix ~prefix:ca_prefix in
    (Fpath.to_string ca_certfile) :: certs
  in
  let output_cert cert =
    let pp =
      if pem
      then Bytesrw_tls.X509_certchain.pp_pem
      else Bytesrw_tls.X509_certchain.pp
    in
    Fmt.pr "@[%a@]@." pp cert
  in
  let dump_cert_file file =
    let* cert = Bytesrw_tls.X509_certchain.read_pem_file file in
    Ok (output_cert cert)
  in
  let* () =
    if not system_ca then Ok () else
    let* system = Bytesrw_tls.X509_certchain.system_ca_certs () in
    begin match system with
    | None -> Log.warn (fun m -> m "No system CA certificates found");
    | Some cert -> output_cert cert
    end;
    Ok ()
  in
  let* () = List.iter_stop_on_error dump_cert_file certs in
  Ok 0

let output_data_dir ~data_dir =
  Log.if_error ~use:Cmdliner.Cmd.Exit.some_error @@
  let* data_dir = get_data_dir ~data_dir in
  Fmt.pr "%a@." Fpath.pp data_dir;
  Ok 0

open Cmdliner
open Cmdliner.Term.Syntax

let envs = [Cmd.Env.info "XDG_DATA_HOME" ~doc:"See option $(b,--data-dir)."]

let data_dir =
   let docs = Manpage.s_common_options in
   let doc = "$(docv) is the data directory." in
   let absent = "$(b,XDG_DATA_HOME)/$(tool) directory" in
   Arg.(value & opt (some B0_std_cli.dirpath) None &
        info ["data-dir"] ~absent ~doc ~docs)

let dry_run =
  let doc = "Do not execute commands print them on $(b,stdout)." in
  Arg.(value & flag & info ["dry-run"] ~doc)

let force =
  let doc = "Force writing files if they exist." in
  Arg.(value & flag & info ["f"; "force"] ~doc)

let prefix =
  let doc =
    "$(docv) is the file path prefix where certificate and private key are \
     written. This writes the files $(docv)-cert.pem and $(docv)-key.pem.
     Directories are created."
  in
  let docv = "PATHPREFIX" in
  Arg.(required & pos 0 (some B0_std_cli.path) None & info [] ~doc ~docv)

let pem =
  let doc = "Output the certificate in PEM format." in
  Arg.(value & flag & info ["pem"] ~doc)

let ca_cmd =
  let doc = "Manage your own certificate authority" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) manages your own certificate authority.";
    `Pre "$(cmd) $(b,create)   # Create the CA certificate (valid for 47 days)";
    `Noblank;
    `Pre "$(cmd) $(b,info)     # Output the CA certificate information";
    `Noblank;
    `Pre "$(cmd) $(b,install)  # Install in the system (sudo may be needed)";
    `Noblank;
    `Pre "$(cmd) $(b,create --force)  # Renew";
  ]
  in
  let ca_create_cmd =
    let doc = "Create the certificate authority certificate" in
    let man = [
      `S Manpage.s_description;
      `P "$(cmd) generates a self-signed certificate for the certificate \
          authority valid for 47 days in the data directory.";]
    in
    Cmd.make (Cmd.info "create" ~man ~doc ~envs) @@
    let+ data_dir and+ force
    and+ name =
      let doc = "$(docv) is the certificate authority name." in
      let docv = "NAME" in
    let absent = "User login name" in
      Arg.(value & opt (some string) None &
         info ["n"; "issuer-name"] ~doc ~docv ~absent)
    in
    ca_create ~data_dir ~name ~force
  in
  let ca_cert_file_cmd =
    let doc = "Output path to the certificate of the certificate authority" in
    Cmd.make (Cmd.info "cert-file" ~doc ~envs) @@
    let+ data_dir in
    ca_cert_file ~data_dir
  in
  let ca_install_cmd =
    let doc = "Install the certificate authority certificate in the system" in
    let man = [
      `S Manpage.s_description;
      `P "$(cmd) tries to make the certificate authority trusted by \
          the system by installing the certificate authority certificate. \
          The procedure depends on the platform, you will likely have \
          to $(b,sudo). Use option $(b,--dry-run) to see what it entails.";
      `P "After having installed you should be able to spot the certificate \
          in the list output by $(tool) $(b,info --system-ca).";
      `P "At the moment there is no $(tool) command to uninstall the \
          certificate. Use your system tools directly to do that.";]
    in
    Cmd.make (Cmd.info "install" ~doc ~man ~envs) @@
    let+ data_dir and+ dry_run and+ force in
    ca_install ~data_dir ~dry_run ~force
  in
  let ca_delete_cmd =
    let doc = "Delete the certificate authority certificate" in
    let man = [
      `S Manpage.s_description;
      `P "$(cmd) delete the self-signed certificate of the certificate \
          authority stored in the data directory (if any).";
      `P "If you installed the certificate with $(cmd.parent) $(b,install) \
          this does not uninstall it."; ]
    in
    Cmd.make (Cmd.info "delete" ~man ~doc ~envs) @@
    let+ data_dir in
    ca_delete ~data_dir
  in
  let ca_info_cmd =
    let doc = "Output information about the certificate authority" in
    Cmd.make (Cmd.info "info" ~doc ~envs) @@
    let+ data_dir and+ pem in
    ca_info ~data_dir ~pem
  in
  Cmd.group (Cmd.info "ca" ~doc ~man) @@
  [ca_create_cmd; ca_cert_file_cmd; ca_info_cmd; ca_install_cmd; ca_delete_cmd]

let ca_signed_cmd =
  let doc = "Generate a certificate signed by your certificate authority" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) generates a certificate signed by your certificate authority \
        and valid for 47 days. The subject name is $(b,localhost) by default. \
        For this to work you need to invoke $(cmd.parent) $(b,ca create) \
        first.";
    `P "The certificate file has the whole certificate chain. Usually this \
        is what your server need to present to browsers.";
    `P "$(cmd) $(b,myapp) # Generate myapp.{cert,key}.pem"; ]
  in
  Cmd.make (Cmd.info "ca-signed" ~doc ~man ~envs) @@
  let+ data_dir and+ prefix and+ force
  and+ only_cert =
    let doc =
      "Do not include the full certificate chain in the certificate \
       PEM file."
    in
    Arg.(value & flag & info ["only-cert"] ~doc)
  and+ name =
    let doc =
      "$(docv) is the subject name. This is used both as the CN \
       subject name and as a subject alternative DNS name so it should \
       be the name for your host"
    in
    let docv = "NAME" in
    Arg.(value & opt string "localhost" & info ["n"; "name"] ~doc ~docv)
  in
  ca_signed ~data_dir ~name ~prefix ~only_cert ~force

let data_dir_cmd =
  let doc = "Output the path to the data directory" in
  Cmd.make (Cmd.info "data-dir" ~doc ~envs) @@
  let+ data_dir in
  output_data_dir ~data_dir

let info_cmd =
  let doc = "Output information about certificates" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) outputs basic information about given certificates.";
    `Pre "$(cmd) $(b,--ca)        # $(tool)'s certificate authority"; `Noblank;
    `Pre "$(cmd) $(b,--system-ca) # CA found in the system"; ]
  in
  Cmd.make (Cmd.info "info" ~doc ~man ~envs) @@
  let+ data_dir and+ pem
  and+ certs =
    let doc =
      "$(docv) is the certificate in PEM format to read. Repeatable."
    in
    let docv = "CERT.pem" in
    Arg.(value & pos_all filepath [] & info [] ~doc ~docv)
  and+ ca =
    let doc =
      "Add the certificate of the $(tool) certificate authority to the list."
    in
    Arg.(value & flag & info ["ca"] ~doc)
  and+ system_ca =
    let doc =
      "Add the certificates of the system certificate authorities to the list. \
       These are the certificates as found by the underlying library."
    in
    Arg.(value & flag & info ["s"; "system-ca"] ~doc)
  in
  info ~data_dir ~certs ~ca ~system_ca ~pem

let self_signed_cmd =
  let doc = "Generate a self-signed certificate" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) generates a self-signed certificate valid for 47 days. \
        The issuer name is equal to the subject name and is $(b,localhost) \
        by default.";
    `Pre "$(cmd) $(b,myapp) # Generate myapp.{cert,key}.pem"; `Noblank;
    `Pre "$(cmd) $(b,-n example.org myapp)";
  ]
  in
  Cmd.make (Cmd.info "self-signed" ~man ~doc) @@
  let+ prefix
  and+ is_ca =
    let doc = "Indicate that this is a self-signed certificate authority." in
    Arg.(value & flag & info ["is-ca"] ~doc)
  and+ name =
    let doc =
      "$(docv) is the issuer and subject CN name. Unless $(b,--is-ca) is \
       specified this is also used to define a subject DNS alternative name \
       so it should be the name of your host"
    in
    let docv = "NAME" in
    Arg.(value & opt string "localhost" & info ["n"; "name"] ~doc ~docv)
  and+ force in
  self_signed ~is_ca ~name ~force ~prefix

let cmd =
  let doc = "Easy X509 certificate management for development" in
  let man = [
    `S Manpage.s_description;
    `P "$(cmd) manages x509 certificates for developement. It generates \
        self-signed certificates or certificate signed by your own \
        certificate authority.";
    `Pre "$(cmd) $(b,self-signed myapp)  # Generate myapp.{cert,key}.pem";
    `Noblank;
    `Pre "$(cmd) $(b,ca create)        # \
          Generate CA cert in the data directory";
    `Noblank;
    `Pre "$(cmd) $(b,ca install)       # \
          Install CA cert as a trusted CA in the system";
    `Noblank;
    `Pre "$(cmd) $(b,ca-signed myapp)  # \
          Generate CA signed myapp.{cert,key}.pem";
  ]
  in
  Cmd.group (Cmd.info "certown" ~version:"%%VERSION%%" ~man ~doc) @@
  [ca_cmd; ca_signed_cmd; data_dir_cmd; info_cmd; self_signed_cmd; ]

let main () = Cmd.eval' cmd
let () = if !Sys.interactive then () else exit (main ())
