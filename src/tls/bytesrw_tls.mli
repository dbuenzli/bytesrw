(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** TLS encrypted streams (via [conf-mbedtls])

    This module provides support for reading and writing TLS encrypted
    streams. It depends on the [bytesrw.crypto] library in
    and is thus subject to its {{!Bytesrw_crypto}automatic
    initialization}.

    {b Backend.} The concrete TLS implementation is abtracted away. It
    is currently provided by the
    {{:https://www.trustedfirmware.org/projects/mbed-tls/}[Mbed TLS]} C
    library.

    {b Sample code.} Have a look the
    {{:https://github.com/dbuenzli/bytesrw/blob/main/test/min_tls.ml}
    [min_tls.ml]},
    {{:https://github.com/dbuenzli/bytesrw/blob/main/test/webfetch.ml}
    [webfetch.ml]} and
    {{:https://github.com/dbuenzli/bytesrw/blob/main/test/webserve.ml}
    [webserve.ml]} examples in the source repository and read a few
    {{!certtips}certificate tips}. *)

(** {1:certs_and_conf Certificates and configuration} *)

(** X.509 certificate chains. *)
module X509_certchain : sig

  (** {1:certchains Certificate chains} *)

  type t
  (** The type for a certificate chain or a set of certificate.

      {b Order}. This may be a proper chain or not. When used as a
      chain, the certificates are assumed to be ordered as a list from
      leaf to root. If it's not a chain but for example the set of
      trusted certificate authorities of a client, the order does not
      matter. *)

  val concat : t list -> t
  (** [concat css] appends the certificates chains [css] from left to
      right. If the result is ordered the leaf is the leaf of [List.hd
      css].

      @raises Invalid_argument if [css] is empty. *)

  val leaf : t -> t
  (** [leaf cs] is the leaf certificate of [c] as a singleton chain. *)

  val fold : ('a -> t -> 'a) -> 'a -> t -> 'a
  (** [fold f acc cs] folds [f] over the individual certificates of [cs]
      starting with [acc]. *)

  (** {2:pem PEM format}

      PEM is plain text with DER certificates encoded in Base64
      surrounded by specific begin/end headers. It can contain lists
      of certificates. If those are meant to be ordered they are
      expected to be read in leaf to root order. PEM files can be
      easily concatenated by separating them with newlines. *)

  val of_pem : ?file:string -> string -> (t, string) result
  (** [of_pem s] parses a certificate chain from bytes [s] which are
      assumed to be in PEM format and have been read from [file]
      (defaults to ["-"], only used for error messages). If you expect
      a chain, the certificates must be ordered from leaf to root in
      the data. *)

  val to_pem : t -> string
  (** [to_pem cs] is the certificate chain in PEM format. If the chain
      is ordered the result is ordered from leaf to root. *)

  val read_pem_file : string -> (t, string) result
  (** [read_pem_file file] reads certificates from [file] in PEM format
      using {!of_pem}. The filename ["-"] can be used to read from [stdin].

      {b Note.} Material loaded by this function can be seen by the GC. *)

  (** {2:der DER format} *)

  val of_der_certs : string list -> (t, string) result
  (** [of_der_certs der_certs] is a certificate chain from the list of
      individual certificates in DER format. If those are meant to be
      ordered they must be sorted from leaf to root.

      @raise Invalid_argument if [der_certs] is empty. *)

  val fold_der_certs : ('acc -> string -> 'acc) -> 'acc -> t -> 'acc
  (** [fold_der_certs f acc cs] folds with [f] over the raw certificates
      in DER format of [cs]. If the certificate are ordered they
      are fold over from leaf to root. *)

  (** {2:system_ca System CA certificates} *)

  val system_ca_certs : unit -> (t option, string) result
  (** [system_ca_certs ()] tries to load the set of certificates of
      certification authorities (CA) that are trusted by your
      operating system using the procedure mentioned below. [None] is
      returned if none can be found.

      The lookup is peformed as follows, in order.

      {ol
      {- If the SSL_CERT_FILE is set and non-empty, the certificates
         are loaded from the specified PEM file with {!of_pem}. Note
         that such a file can be easily downloaded from
         {{:https://curl.se/docs/caextract.html}curl's CA certificate extract}.}
      {- On Windows it tries to load them from the ["CA"] certificate store.}
      {- On macOS it tries do load those returned by invoking
         {{:https://developer.apple.com/documentation/security/sectrustsettingscopycertificates(_:_:)?language=objc}[SecTrustSettingsCopyCertificates]} on
         all domains (system, admin and user).}
      {- Otherwise it reads the first file that exists from the
         following list:
         {ul
         {- [/etc/ssl/certs/ca-certificates.crt]}
         {- [/etc/ssl/cert.pem]}}}} *)

  (** {1:owncert Own certificate chains} *)

  (** Certificate private keys. *)
  module Private_key : sig

    type t
    (** The type for certificate private keys.

        {b Note.} The private key data is kept in a C datastructure
        so the GC does not see it. *)

    val read_pem_file : string -> (t, string) result
    (** [of_pem_file file] loads a private key from file [file].

        {b Note.} This function ensures that the GC does not see the
        private key data. *)

    val write_pem_file : string -> t -> (unit, string) result
    (** [write_pem_file file k] writes the private key [k] to [file].

        {b Note.} This function ensures that the GC does not see the
        private key data. *)

    val generate : unit -> (t, string) result
    (** [generate ()] generates a suitable private key whose details
        are subject to change. Currently this is an ECDSA
        secp256r1 (P-256) private key.  *)

    val copy_psa_key : Bytesrw_crypto.Psa.Key_id.t -> (t, string) result
    (** [copy_psa_key k] is a certificate private key from the given
        PSA key. This key must be
        {{!Bytesrw_crypto.Psa.Key_usage.export}exportable} and be
        an RSA or elliptic curve key. The psa key can be destroyed
        afterwards. *)
  end

  type own = t * Private_key.t
  (** An own certificate is a certificate chain for which we have
      the private key for the leaf certificate.

      {b Note.} It's your duty to make sure the private and public key
      in the leaf certificate match. If they don't handshakes using the
      certificate will fail. *)

  (** {2:self_signing Self-signing}

      These are simple functions that can be used for generating
      certificates for development, see the {{!Bytesrw_tls.certtips}
      certificate tips}. They provide little control on the
      certificate fields. If you need fine grained control you may
      want to use something like
      {{:https://github.com/mirleft/ocaml-x509}[ocaml-x509]} but
      in practice you will likely get your certificates ready-made from
      {{:https://certbot.eff.org/}certbot}. *)

  type ptime = float
  (** The type POSIX time as returned by {!Unix.gettimeofday}. *)

  val self_signed :
    ?private_key:Private_key.t -> ?invalid_before:float ->
    ?invalid_after:float -> ?is_ca:bool -> ?name:string -> unit ->
    (own, string) result
  (** [self_signed ~name ()] returns a self-signed certificate with:
      {ul
      {- [name], the issuer and subject CN name. If [ca] is [false] this
         is also used to define a DNS subject alternative name, so it
         should be the DNS name of your host. Defaults to [localhost].}
      {- [is_ca], if [true] generates a certification authority
         certificate. This certificate is able to sign other certificates,
         see {!ca_signed}. Defaults to [false].}
      {- [invalid_before], the time before which it is invalid. Defaults to
         current time as determined by {!Unix.gettimeofday}.}
      {- [invalid_after], the time after which it is invalid. Defaults to
         47 days after [invalid_before]}
      {- [private_key], if provided this is used for the private key. Otherwise
         a new one is generated with {!Private_key.generate}.}} *)

  val ca_signed :
    ca:own -> ?private_key:Private_key.t -> ?invalid_before:float ->
    ?invalid_after:float -> ?name:string -> unit -> (own, string) result
  (** [ca_signed] returns a certificate chain with the chain of [ca] and
      a new certificate signed by [ca] with:
      {ul
      {- [name], the subject CN name and the DNS subject alternative
         name so it should be the DNS name of your host. Defaults to
         ["localhost"].}
      {- [invalid_before], the time before which it is invalid. Defaults to
         current time as determined by {!Unix.gettimeofday}.}
      {- [invalid_after], the time after which it is invalid. Defaults to
         47 days after [invalid_before].}
      {- [private_key], if provided this is used for the private key. Otherwise
         a new one is generated with {!Private_key.generate}.}} *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats a certificate chain for inspection in leaf to
      root order (if applicable). *)

  val pp_pem : Format.formatter -> t -> unit
  (** [pp] formats a certificate chain in PEM format in leaf to root
      order (if applicable). *)
end

(** TLS connection configuration. *)
module Conf : sig

  type tls_version = Tls_v1_2 | Tls_v1_3 (** *)
  (** The type for TLS versions. *)

  type kind =
  | Client (** Configuration for talking to a server. *)
  | Server (** Configuration for talking to clients. *)
  (** The type for kind of TLS configurations. *)

  type t
  (** The type for configuring TLS connections. *)

  val make :
    ?alpn_protocols:string list -> ?min_tls_version:tls_version ->
    ?max_tls_version:tls_version -> ?own_certs:X509_certchain.own list ->
    ?trusted_certs:X509_certchain.t list -> ?verify_peer:bool -> kind ->
    (t, string) result
  (** [make kind] is a TLS configuration with given properties,
      ee corresponding accessors for semantics. The same configuration can
      used for multiple TLS connections. The defaults are as follows:
      {ul
      {- {!alpn_protocols} defaults to [[]].}
      {- {!min_tls_version} defaults to [Tls_v1_2].}
      {- {!max_tls_version} defaults to [Tls_v1_3].}
      {- If [kind] is Client:
         {ul
         {- {!own_certs} defaults to [[]].}
         {- {!verify_peer} defaults to [true]. {b Warning.} Setting this to
            [false] is insecure.}
         {- {!trusted_certs} defaults to {!X509_certchain.system_ca_certs} if
            {!verify_peer} is [true] and [[]] otherwise.}}}
      {- If [kind] is Server:
         {ul
         {- {!own_certs} has no default. A non-empty list {b must} be provided
            otherwise the function errors.}
         {- {!verify_peer} defaults to [false].}
         {- {!trusted_certs} defaults to [[]].}}}}
       *)

  val alpn_protocols : t -> string list
  (** [alpn_protocols c] is the list of supported protocols
      identifiers for the connection in decreasing preference
      order. See the
      {{:https://www.iana.org/assignments/tls-extensiontype-values/tls-extensiontype-values.xhtml#alpn-protocol-ids}IANA list of ALPN protocols}. *)

  val kind : t -> kind
  (** [kind c] is the kind of TLS configuration of [c]. *)

  val min_tls_version : t -> tls_version
  (** [min_tls_version c] is the minimal TLS version of [c]. *)

  val max_tls_version : t -> tls_version
  (** [max_tls_version c] is the maximal TLS version of [c]. *)

  val own_certs : t -> X509_certchain.own list
  (** [own_certs c] are the certificates for this host.

      This must be given for servers. Note that browsers usually
      expect the full certification chain. For clients this can be
      provided in order to perform mutual TLS, in this case only the
      first certificate in the list is taken into account. *)

  val trusted_certs : t -> X509_certchain.t list
  (** [trusted_certs c] is the set of certificates that are used to
      verify the peer's certificate. *)

  val verify_peer : t -> bool
  (** [verify_peer c] is [true] if the peer is verified with
      {!trusted_certs}. If [kind c] is [Client] and this is [false]
      the connection is insecure. *)

  (** {1:fmt Formatters} *)

  val pp_tls_version : Format.formatter -> tls_version -> unit
  (** [pp_tls_version] formats TLS versions for inspection. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_lind] formats configuration kinds for inspection. *)
end

(** TLS connection information.

    {b Warning.} Connection information is available only while
    the connection is active. Trying to access it afterwards raises
    [Invalid_argument]. *)
module Info : sig
  type t
  (** The type for information about TLS connections. *)

  val tls_version : t -> Conf.tls_version option
  (** [tls_version i] is the TLS version used by the connection. *)

  val alpn_protocol : t -> string option
  (** [alpn_protocol i] is the name of the negociated application
      layer protocol (if any). *)

  val peer_cert : t -> X509_certchain.t option
  (** [peer_cert i] is the certificate of the peer, if available. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats connection information for inspection. *)

  val pp_peer_cert : Format.formatter -> t -> unit
  (** [pp_peer_cert] formats the peer certificate for inspection or
      a message if there is none. *)

  val pp_peer_cert_pem : Format.formatter -> t -> unit
  (** [pp_peer_cert_pem] formats the peer certificate in PEM
      or a message if there is none. *)
end

val backend_info : unit -> string
(** [backend_info ()] is a string that indicates the underyling
    library and version implementing the TLS support. *)

(** {1:streams Streams} *)

open Bytesrw

type Bytes.Stream.error += Error of string (** *)
(** The type for TLS streams errors.

    Readers and writers using TLS encrypted streams may
    {!Bytes.Stream.Error} with this error. *)

val for_client_socket :
  Conf.t -> peer_hostname:string -> peer:Unix.file_descr ->
  (Info.t -> peer:(Bytes.Writer.t * Bytes.Reader.t) -> 'a) ->
  ('a, string) result
(** [for_client_socket conf ~peer_hostname ~peer:fd f] setups a TLS stream on
    [fd] according to [conf] assuming it contacts [peer_hostname] and
    the [fd] is {!Unix.connect}ed to the IP address of that peer.
    Unless {!Conf.verify_peer}[ conf] is [false], the peer certificate
    is verified using the {!Conf.trusted_certs} certificates.  If
    {!Conf.own_certs} is present the first of these certificates is
    presented to the peer for potential authentication (mutual TLS).

    If the connection setup fails, [Error _] is returned. Otherwise [f] is
    called with information about the connection and a pair [(send,
    recv]) of bytes writer and reader. Writing plain data on [send]
    sends it encrypted to the peer and reading from [recv] reads
    decrypted data received from the peer.

    Both [info] and [(send, recv)] are only valid during the call to
    [f] and using them afterwards raises [Invalid_argument].

    @raise Invalid_argument if [Conf.kind conf <> Client]. *)

val for_server_socket :
  Conf.t -> peer:Unix.file_descr ->
  (Info.t -> peer:(Bytes.Writer.t * Bytes.Reader.t) -> 'a) ->
  ('a, string) result
(** [for_server_socket conf ~peer:fd f] setups a TLS stream on [fd]
    according to [conf] assuming [fd] has been {!Unix.accept}ed from a
    listening socket. If {!Conf.verify_peer}[ conf] is [true], the
    peer must present a certificate which can be verified using the
    {!Conf.trusted_certs} certificates.

    If the connection setup fails, [Error _] is returned. Otherwise [f]
    is called with the information about the connection and a pair [(send,
    recv)] of bytes writer and reader. Writing plain data on [send] sends
    it encrypted to the peer and reading from [recv] reads decrypted data
    received from the peer.

    Both [info] and [(send, recv)] are only valid during the call to
    [f] using them afterwards raises [Invalid_argument].

    The {!Conf.own_certs} certificate is given to the peer for
    verification.

    @raise Invalid_argument if [Conf.kind conf <> Server]. *)

(** {1:certtips Certificate tips for development}

    The certificate business may get in the way when you develop servers.
    Here are a few strategies.

    {2:self-signed Self-signed certificates}

    You can easily generate self-signed certificates with
    {!X509_certchain.self_signed} function. For example if no
    certificate is specifed on the command line, the
    {{:https://github.com/dbuenzli/bytesrw/blob/main/test/webserve.ml}
    [webserve.ml]}
    example generates one dynamically for [localhost] and outputs it
    on stdout which you can then read e.g. from [curl] or instruct
    your browser to trust them when they hit them.

    {@sh[
    webserve > /tmp/cert.pem
    curl --cacert /tmp/cert.pem https://localhost:4443
    ]}

    The [certown] tool distributed with the library can also be used
    to generate these certificates for example:

    {@sh[
    certown self-signed /tmp/myapp
    webserve --own-cert /tmp/myapp.cert.pem --own-key /tmp/myapp.key.pem
    curl --cacert /tmp/myapp.cert.pem https://localhost:4443
    ]}

    {2:becoming-a-ca Become your own certificate authority}

    A self-signed certificate for a certificate authority can be
    created with {!X509_certchain.self_signed}[ ~is_ca:true]. Be
    careful with the private key of this certificate, you need to
    protect it.

    Add this certificate to the trusted certificate authorities (CA)
    of your system or browser. Equiped with the CA certificate and
    private key you can generate certificates signed by it with
    {!X509_certchain.ca_signed}. These should then validate in
    browsers without more operations.

    The [certown] tool distributed with the library helps to
    streamline this workflow. For example the following create a CA
    certificate (valid for 47 days) and private key in the
    [~/.local/share/certown] directory and tries to install the
    certificate as a system trusted CA.

{@sh[
certown ca create   # Creates a CA cert in ~/.local/share/certown
certown ca install  # Install the CA cert in your system, sudo may be needed
certown ca-signed /tmp/myapp # Generate certificate (chain) signed by the CA
webserve --own-cert /tmp/mycert.cert.pem --own-key /tmp/mycert.key.pem
curl https://localhost:4443
]}

*)
