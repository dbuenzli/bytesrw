(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Extended {!Stdlib.Bytes} modules.

    Open the module to use it. This adds these modules:
    {!modules:
    Bytes.Slice
    Bytes.Reader
    Bytes.Writer}
    to the {!Stdlib.Bytes} module. *)

(** Extended {!Stdlib.Bytes} module. *)
module Bytes : sig

  include module type of Stdlib.Bytes (** @closed *)

  (** Bytes slices.

      A bytes slice is a {e non-empty} consecutive range of bytes in a
      {!Bytes.t} value. The special empty slice {!Slice.eod} is used to indicate
      end of data. *)
  module Slice : sig

    (** {1:validity Validity}

        The range of a slice is made available to a third-party for a
        limited amount of time during which the slice is said to be {e
        valid} for reading or writing (or both). Third parties are
        only allowed to access the bytes in the range and in the mode
        specified while it is valid. *)

    (** {1:slices Slices} *)

    type t
    (** The type for byte slices. *)

    val make : bytes -> first:int -> length:int -> t
    (** [make b ~first ~length] are the bytes of [b] in the range
        \[[first]; [first+length-1]\].

        Raises [Invalid_argument] if [length] is not positive, larger
        than the length of [b] or if [first] is out of bounds. *)

    val bytes : t -> bytes
    (** [bytes s] are the underlying bytes of the slice [s]. *)

    val first : t -> int
    (** [first s] is the index of the first byte of [s]. *)

    val length : t -> int
    (** [length s] is the length in bytes of [s]. *)

    (** {1:end_of_data End of data} *)

    val eod : t
    (** [eod] is a slice to denote the end of data. It is the only slice
        with [length d = 0] and [bytes eod == Bytes.empty]. *)

    val is_eod : t -> bool
    (** [is_eod s] is [true] iff [s == eod]. *)

    (** {1:converting Converting} *)

    val to_string : t -> string
    (** [to_string s] is [s] as a string. *)
  end

  (** Bytes readers.  *)
  module Reader : sig

    (** {1:readers Bytes readers} *)

    type t = unit -> Slice.t
    (** The type for bytes readers.  TODO abstract. *)

    val make : (unit -> Slice.t) -> t
    (** [make rf] is a bytes reader from the function [rf].

        The [rf] function is called by the reader to read bytes
        and must be such that:
        {ul
        {- [rf ()] returns {!Slice.t} values until it returns
           a {!Slice.eod}.}
        {- Returned slices must be {{!Slice.validity}valid for reading}
           until the next call to [rf ()].}
        {- Once {!Slice.eod} is returned, further call to [rf] always
           return {!Slice.eod}}} *)

    (** {1:reads Reading} *)

    val read : t -> Slice.t
    (** [read br] reads the next slice from [br]. The slice is valid for
        reading until the next call to [read] on [br]. Once {!Slice.eod} is
        returned, {!Slice.eod} is always returned. *)

    (** {1:convert Converting} *)

    val of_in_channel : ?buf:bytes -> In_channel.t -> t
    (** [of_in_channel ic], sets [ic] in binary mode and reads the
        bytes of [ic]. [buf] is used as the internal buffer, it
        defaults to [Bytes.create 65536]. *)

    val of_string : ?slice_length:int -> string -> t
    (** [of_string s] reads the bytes of [s]. [slice_length] is the
        maximal length of slices, it defaults to [String.length s]. *)

    val of_bytes : ?slice_length:int -> bytes -> t
    (** [of_bytes b] is like {!of_string} but reads the bytes of [b]. *)

    val to_string : t -> string
    (** [to_string r] reads [r] until {!Slice.eod} into a string [s]. *)

    val to_buffer : Buffer.t -> t -> unit
    (** [to_buffer b r] reads [r] and adds its slices to [b] until
        {!Slice.eod}. *)
  end

  (** Bytes writers. *)
  module Writer : sig

    type t = Slice.t -> unit
    (** The type for bytes writers. TODO abstract *)

    val make : (Slice.t -> unit) -> t
    (** [make wf] is a bytes writer from the function [wf].

        The [wf] function is called by the writer to write bytes as follows:
        {ul
        {- [wf s] is called with {!Slice.t} values until it is called
           with {!Slice.eod}.}
        {- Slice values given to [wf] are {{!Slice.validity}valid for
           reading} until the [wf] function returns.}
        {- Once [wf] was called with {!Slice.eod} it will
           only ever be called with {!Slice.eod}}} *)

    (** {1:writing Writing} *)

    val write_string : ?slice_length:int -> t -> string -> unit
    (** [write_string w s] writes the bytes [s] on [w]. [slice_length]
        is the maximal length of slices, it defautls to [String.length s].
        It {b does not} write a final {!Slice.eod}, use {!write_eod}
        for that. *)

    val write_bytes : ?slice_length:int -> t -> bytes -> unit
    (** [write_bytes w b] is like {!write_string} but writes the bytes of
        [b]. *)

    val write_eod : t -> unit
    (** [write_eod w] is [w Slice.eod]. *)

   (** {1:convert Converting} *)

    val of_out_channel : Out_channel.t -> t
    (** [of_out_channel oc] sets [oc] to binary mode and writes bytes
        to [oc]. [oc] is {{!Out_channel.flush}flushed} on
        {!Slice.eod}. *)

    val of_buffer : Buffer.t -> t
    (** [of_buffer b] writes bytes to [b]. *)
  end
end
