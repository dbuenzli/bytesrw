(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cryptographically secure pseudorandom byte streams and entropy.

    This module provides cryptographically secure pseudorandom bytes
    and an entropy primitive using operating system sources.  See the
    documentation of the {{!primitives}primitives} for details on the
    sources. *)

open Bytesrw

(** {1:csprng Cryptographically secure pseurorandom bytes} *)

val reads :
  ?pos:Bytes.Stream.pos -> ?slice_length:Bytes.Slice.length -> ?length:int ->
  unit -> Bytes.Reader.t
(** [reads ()] is a stream of [length], or unbounded if unspecified,
    cryptographically secure pseudorandom bytes in [slice_length]
    chunks (defaults to {!Bytesrw.Bytes.Slice.default_length}).

    Reads may raise {!Panic} which is not turned into a stream
    error as it likely indicates a serious condition in the system,
    see the underlying call {!set_random}. *)

val string : int -> string
(** [string n] are [n] cryptographically secure pseudorandom
    bytes. Raises {!Panic} in case of problems, see the underlying
    call {!set_random}. *)

val bytes : int -> bytes
(** [bytes n] are [n] cryptographically secure pseudorandom
    bytes. Raises {!Panic} in case of problems,
    see the underlying call {!set_random}. *)

(** {1:primitives Primitives} *)

exception Panic of string
(** Exception raised by primitives in case of problem. If this happens
    do not try to handle the exception, log it at the toplevel of your
    program and abort the program. It likely indicates a serious
    condition in the system. *)

(** {2:primitive_csprng Cryptographically secure pseudorandom bytes} *)

val set_random : Bytes.Slice.t -> unit
(** [set_random s] writes the bytes in the slice range with
    cryptographically secure pseudorandom bytes. Theoretically this
    function should not block or error, except perhaps if you try to
    use it in early OS boot phase.

    This uses:
    {ul
    {- {{:https://www.man7.org/linux/man-pages/man2/getrandom.2.html}
       [getrandom]} on Linux.}
    {- {{:https://man.openbsd.org/arc4random.3}[arc4random_buf]} on
       other Unixes.}
    {- {{:https://docs.microsoft.com/en-us/windows/win32/api/ntsecapi/nf-ntsecapi-rtlgenrandom}[RtlGenRandom]} on Windows
       ({{:https://bugzilla.mozilla.org/show_bug.cgi?id=504270}safe to use}
       despite the availability warning).}
    {- A stub that unconditionally raises {!Panic} otherwise.}}

    Raises {!Panic} in case of problem, not meant to be handled. *)

(** {2:primitive_entropy Entropy} *)

val set_entropy : Bytes.Slice.t -> unit
(** [set_entropy s] write the bytes in the slice range with entropy
    from your operating system. The function {b blocks} until enough
    entropy is gathered. The {!Bytesrw.Bytes.Slice.val-length} of [s] must
    be smaller or equal to 256 or [Invalid_argument] is raised.

    This uses:
    {ul
    {- {{:https://pubs.opengroup.org/onlinepubs/9799919799/functions/getentropy.html}[getentropy]} on POSIX systems.}
    {- {{:https://docs.microsoft.com/en-us/windows/win32/api/ntsecapi/nf-ntsecapi-rtlgenrandom}[RtlGenRandom]} on Windows
       ({{:https://bugzilla.mozilla.org/show_bug.cgi?id=504270}safe to use}
       despite the availability warning).}
    {- A stub that unconditionally raises {!Panic} otherwise.}}

    Raises {!Panic} in case of problem, not meant to be handled. *)
