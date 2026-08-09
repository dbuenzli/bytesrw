(*---------------------------------------------------------------------------
   Copyright (c) 2026 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** WebSocket connections.

    This module provides an
    {{:https://www.rfc-editor.org/rfc/rfc6455.html}RFC 6455} WebSocket
    connection abstraction encapsulating a user provided byte stream reader
    and writer to exchange messages with a peer. It can be used
    both for clients and servers.

    The abstraction only handles the protocol
    after the HTTP-based
    {{:https://www.rfc-editor.org/rfc/rfc6455.html#section-1.3}
    opening handshake} has been performed. Support for the latter depends
    on your HTTP library and can be found, for example, in
    the {!Webs_websocket} module.

    Read the {{!features_and_limitations}features and limitations}.

    {b Warning.} This module is experimental and subject to change
    it may be moved somewhere else.
*)

open Bytesrw

(** {1:synchronization Synchronization} *)

type synchronized = { synchronized : 'a. (unit -> 'a) -> 'a }
(** The type for specifying mutual exclusion primitives.

    Given a critical section [f] the [synchronized] function must ensure
    that [f ()] cannot be called concurrently until it returns (including
    by raising an exception). For example this function creates primitives
    for system threads:
    {[
    let make_synchronized : unit -> synchronized = fun ()
      let m = Mutex.create () in
      { synchronized = fun f -> Mutex.protect m f }
    ]} *)

(** {1:errors_and_status_codes Errors and status codes} *)

type error =
| Shutdown
(** The connection is closed or closing. Message {{!sending}sending} and
    {{!receiving}receiving} functions raise this stream error when the
    connection is closed or closing. The latter occurs either because
    the peer sent a close frame or because the peer is
    misbehaving. This error must be handled by closing the connection
    with {!val-close} which will provide more human-readable
    information. *)
| Recv_message_byte_size_exceeded of int
(** The given maximal received message byte size is exceeded. Message
    {{!receiving}receiving} functions raise this stream error when
    message size limit specified on {!make} is exceeded.  This error
    must be handled by closing the connection with {!val-close} which
    will provide an error message. *)
(** The type for WebSocket byte stream errors condititions. *)

type Bytesrw.Bytes.Stream.error +=
| Error of error (** *)
(** The type for WebSocket byte stream errors. *)

type status_code = int
(** The type for WebSocket
    {{:https://www.rfc-editor.org/rfc/rfc6455#section-7.4}status codes}. *)

val utf_8_error : status_code
(** [utf_8_error] is {{:https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1}
    1007}. It can be used to indicate on {!val-close}
    that a received text message was UTF-8 invalid. *)

val pp_status_code : Format.formatter -> status_code -> unit
(** [pp_status_code] formats status codes for inspection. This the status code
    number followed by a short human description. *)

(** {1:sides Sides} *)

type side =
| Client (** A client side WebSocket. *)
| Server (** A server side WebSocket. *)
(** The type for WebSocket sides. *)

val pp_side : Format.formatter -> side -> unit
(** [pp_side] formats sides for inspection. *)

(** {1:connections Connections} *)

val default_max_recv_message_byte_size : int
(** [default_max_recv_message_byte_size] is 512KB, the default value used
    on {!make} for {!max_recv_message_byte_size}. *)

type t
(** The type for WebSocket connections. *)

val make :
  ?make_synchronized:(unit -> synchronized) ->
  ?max_recv_message_byte_size:int ->
  ?set_crypto_random:(Bytesrw.Bytes.Slice.t -> unit) ->
  ?close:(unit -> (unit, string) result) -> send:Bytes.Writer.t ->
  recv:Bytes.Reader.t -> side -> t
(** [make ~send ~recv side] is a WebSocket connection with:

    {ul
    {- [send] is the bytes writer used to send data to the peer.
       A {!Bytesrw.Bytes.Slice.eod} is unconditionally written on it
       on {!val-close}.}
    {- [recv] is the bytes reader used to receive data from the peer.
       If {!Bytesrw.Bytes.Slice.eod} is returned
       the connection proceeds to close. If {!val-close} is called
       not all slices may have been consumed in which case, if there is
       no error, the last read byte was the last byte of the last decoded
       frame.}
    {- [side] indicates if this is the server or client side
       of the WebSocket (affects frame masking).}
    {- [close] is called on {!val-close}. It can be used
       to dispose ressources associated to [send] and [recv]. It must not
       raise. It is guaranteed to be called only once. Defaults to
       [Fun.const (Ok ())], if an error is returned it gets into a {!val-close}
       error.}
    {- [set_crypto_random] is used to generate masks for client sent
       frames. It must generate cryptographically secure pseudorandom bytes
       in the given slice. Defaults to {!Bytesrw_sysrandom.set_random}.
       The function is never invoked if [side] is [Server].}
    {- [max_recv_message_byte_size] is the maximal allowed byte size of a
       received message after fragment reassembly. If a received message
       exceeds this size, the connection errors and is closed on our side with
       {{:https://www.rfc-editor.org/rfc/rfc6455#section-7.4.1}1009}.
       Defaults to {!default_max_recv_message_byte_size}. On 64-bit platforms
       using {!Int.max_int} effectively makes the message size unlimited
       (note however that if you use the non-streaming function {!recv} you
        are limited by {!Sys.max_string_length}).}
    {- [make_synchronized ()] must return a new mutual exclusion primitive.
       This function {b must} be provided if you want to perform sends and
       receives concurrently. See {!type-synchronized}.}}

    {b Important.} A WebSocket connection is a ressource it must
    always eventually be closed by {!close}. *)

val max_recv_message_byte_size : t -> int
(** [max_recv_message_byte_size c] is the maximal byte size of received
    messages after fragment reassembly allowed by [c]. *)

val side : t -> side
(** [side c] is the side of [c]. *)

(** {1:closing Closing} *)

val close : ?suggested_status:status_code -> t -> (unit, string) result
(** [close c] closes the connection. Its effects are performed only once
    and thus can be safely called more than once, however an error is
    reported only on the first call. In case of abnormal closure, a human
    readable message is returned which can be used for diagnostics.

    The function does not raise exceptions conditional on the byte streams
    readers and writers {{!make}given} to [c] raising only
    {!Bytesrw.Bytes.Stream.Error} and the [close] function
    {{!make}given} to [c] not raising.

    The first time the function is called it performs the following, in order:
    {ol
    {- Sends a close frame to the peer and writes {!Bytesrw.Bytes.Slice.eod} on
       the [send] bytes writer {{!make}given} to [c] (even if sending the close
       frame raised a stream error). If [suggested_status] is specified and
       there is no particular internal closure status code this status is
       sent to the peer. This can be used with {!utf_8_error} if you are
       angry about the peer sending an invalid UTF-8 encoded text message.}
    {- If the peer did not initiate the closure, it tries to receive a close
       frame from the peer but gives up after at most [1KiB] or [4] frames
       have been read to mitigate denial of service attacks. It is also a good
       idea for the [recv] bytes reader {{!make}given} to [c] to have timeouts
       in place. See {!features_and_limitations}.}
    {- Calls the [close] function given in {!make}.}} *)

val close_status_code : t -> status_code
(** [close_status_code] is the
    {{:https://www.rfc-editor.org/rfc/rfc6455.html#section-7.1.5}WebSocket
    connection close code}. Its value is only known after {!close} was
    called and returned. *)

(** {1:sending Sending} *)

val send : ?text:bool -> t -> string -> unit
(** [send c s] sends on [c] a message made of the bytes [s]. If [text] is
    [true] (defaults to [false]), a text message is sent and you must
    make sure [s] is valid UTF-8 text. This uses a single frame to
    send the message.

    The function raises a {!Shutdown} stream error if
    the connection is closing. In that case nothing was sent and you
    should proceed to {!close} the connection. Other stream
    errors can be raised depending on the [send] bytes writer
    {{!make}given} to [c]. *)

val send_reader : ?text:bool -> t -> Bytes.Reader.t -> unit
(** [send_reader c r] sends on [c] a message made of the slices read from
    [r] until {!Bytesrw.Bytes.Slice.eod} is read. If [text] is [true]
    (defaults to [false]), a text message is sent and you must make
    sure that the slices of [r] are valid UTF-8 text. The framing used
    to send the message is unspecified and subject to change but the
    current implementation uses at least one frame per slice.

    The function raises a {!Shutdown} stream error if
    the connection is closing. In that case [r] is only partially
    read and sent and you should proceed to {!close} the connection.
    Other stream errors can be raised depending on the [send] bytes writer
    {{!make}given} to [c] or [r]. *)

val send_writes : ?text:bool -> t -> (Bytes.Writer.t -> 'a) -> 'a

val send_ping : t -> unit
(** [send_ping c] sends a ping frame to the peer.

    The function raises a {!Shutdown} stream error if
    the connection is closing. In that case nothing was sent and you
    should proceed to {!close} the connection. Other stream
    errors can be raised depending on the [send] bytes writer
    {{!make}given} to [c].

    {b Note.} There is no function to send pongs. Pings are automatically
    acknowledged by {{!receiving}receiving} functions. *)

(** {1:receiving Receiving}

    {b Note.} Receiving functions may also sometimes send to
    automatically acknowledge received ping frames. *)

val last_recv_message_is_text : t -> bool
(** [last_recv_message_is_text c] is [true] if the last received message
    is supposed to be an UTF-8 encoded text message. *)

val recv : t -> string
(** [recv c] receives a message from [c] whose size is bounded by
    {!max_recv_message_byte_size}.

    After the call, {!last_recv_message_is_text} indicates if the message is
    supposed to be UTF-8 text. The UTF-8 validity of the message is not
    checked by the module.

    The function raises a {!Shutdown} stream error if the connection is closing
    or {!Recv_message_byte_size_exceeded} stream error if the message exceeds
    the size specified by {!max_recv_message_byte_size}. In both cases you
    should proceed to {!close} the connection. Other stream errors can be raised
    depending on the [recv] bytes reader and [send] bytes writer
    {{!make}given} to [c]. *)

val recv_reader : t -> Bytes.Reader.t
(** [recv_reader c] is a byte stream reader that receives a message from
    [c] whose size is bounded by {!max_recv_message_byte_size}.

    After the call, {!last_recv_message_is_text} indicates if the
    slices of the byte stream reader are supposed to be UTF-8
    text. The UTF-8 validity of the slices are not checked by
    the module. The message byte size is guaranteed not to
    be larger than {!max_recv_message_byte_size}.

    {b Warning.} The returned byte stream reader must be consumed until
    {!Bytesrw.Bytes.Slice.eod} is returned or the {!Shutdown} stream error
    is raised before performing any other receive operation.

    The function raises a {!Shutdown} stream error if the connection is closing
    or {!Recv_message_byte_size_exceeded} stream error if the message exceeds
    the size specified by {!max_recv_message_byte_size}. In both cases you
    should proceed to {!close} the connection. Other stream errors can be raised
    depending on the [recv] bytes reader and [send] bytes writer
    {{!make}given} to [c]. *)

val write_recv : Bytes.Writer.t -> t -> unit
(** [write_recv w c] receives a message from [c] whose size is
    bounded by {!max_recv_message_byte_size} and writes it on [w]
    (no {!Bytes.Slice.eod} is written on [w]).

    After the call, {!last_recv_message_is_text} indicates if the
    written slices were supposed to be UTF-8 text. The UTF-8 validity
    of the written slices are not checked by the module.

    The function raises a {!Shutdown} stream error if the connection is closing
    or {!Recv_message_byte_size_exceeded} stream error if the message exceeds
    the size specified by {!max_recv_message_byte_size}. In both cases you
    should proceed to {!close} the connection. Other stream errors can be raised
    depending on the [recv] bytes reader and [send] bytes writer
    {{!make}given} to [c]. *)

(** {1:features_and_limitations Features and limitations}

    {ul
    {- {b Unsynchronized accesses.} Individual sends must
       be synchronized by the user of the module at the connection
       level: no two concurrent send should be performed on the connection.
       The same holds for receives. However sending and receiving can be
       performed concurrently if (and only if) a suitable
       [make_synchronized] function to
       create {{!type-synchronized}mutual exclusion primitives} is provided on
       {!make}.}
    {- {b Timeouts.} The connection has no notion of timeout. Connection
       timeouts should be enforced by having the underlying [send] and [recv]
       streams {{!make}given} to the connection raising dedicated stream
       errors. This is especially important for servers to mitigate
       denial of service attacks.}
    {- {b Text messages.} The module does not check the UTF-8 validity
       of text messages. On sending text messages the module user must
       provide valid UTF-8 text. On receiving text messages, the module
       does not check the UTF-8 validity as this is usually handled by the
       higher-level format, e.g. JSON parsing. If a text message is received
       with invalid UTF-8 text, the connection should be closed by the user of
       the module with {!close} and an {!utf_8_error} suggested status.}
    {- {b Ping acknowledgements.} The module automatically acknowledges
       ping frames with a pong frame in {{!receiving}receive functions}.}
    {- {b Close frames.} Only a potential status code in the frame payload
       is considered. A following error message is not as it is
       {{:https://www.rfc-editor.org/rfc/rfc6455.html#section-5.5.1}
       not guaranteed} to be human readable.}
    {- {b 0-sized non-final fragments}. For messages that are fragmented
       we reject, {b in violation of the RFC}, non-final fragment
       messages of size [0]. Otherwise a peer can send an infinite amount
       of frames without triggering the {!max_recv_message_byte_size}
       limit. Perhaps a better idea would be to have a limit
       that includes fragment header sizes, but it makes it harder for users
       to have guarantees at the application level on message sizes
       since it limits how many frames can be used to send a message.}} *)
