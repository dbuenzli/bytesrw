(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Extended {!Stdlib.Bytes} module. Open to use it.

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
      {!Bytes.t} value. The sole distinguished {e empty} slice {!Slice.eod}
      is used to indicate end of data. *)
  module Slice : sig

    (** {1:validity Validity}

        The bytes in the range of a slice is made available to a third-party
        for a limited amount of time during which the slice is said to be {e
        valid} for reading or writing (or both). Third parties are
        only allowed to access the bytes in the range and in the mode
        specified while it is valid. *)

    (** {1:slices Slices} *)

    type t
    (** The type for bytes slices. *)

    val make : bytes -> first:int -> length:int -> t
    (** [make b ~first ~length] are the bytes of [b] in the range
        \[[first]; [first+length-1]\].

        This function does not allow the creation of the empty {!Slice.eod}
        which is a feature. See also {!make_or_eod}, {!of_bytes} and
        {!of_bytes_or_eod}.

        Raises [Invalid_argument] if [length] is not positive, larger
        than the length of [b] or if [first] is out of bounds. *)

    val make_or_eod : bytes -> first:int -> length:int -> t
    (** [make_or_eod] is like {!make} but returns {!eod} instead of
        raising [Invalid_argument] if [length] is not positive. *)

    val copy : tight:bool -> t -> t
    (** [copy ~tight s] is a copy of [s]. If [tight] is [true], the
        copy contains only the bytes in the range of [s]. If not the
        whole [bytes s] is copied. *)

    (** {1:end_of_data End of data} *)

    val eod : t
    (** [eod] is a slice to denote the end of data. It is the only slice
        with [length d = 0] and [bytes eod == Bytes.empty]. *)

    val is_eod : t -> bool
    (** [is_eod s] is [true] iff [s == eod]. *)

    (** {1:props Properties} *)

    val bytes : t -> bytes
    (** [bytes s] are the underlying bytes of the slice [s]. *)

    val first : t -> int
    (** [first s] is the index of the first byte of [s]. *)

    val length : t -> int
    (** [length s] is the byte length of [s]. *)

    (** {1:breaking Breaking slices} *)

    val take : int -> t -> t
    (** [take n s] is the slice made of the first [n] bytes of [s]
        This is {!eod} if [n <= 0] and [s] itself if [n >= length s]. *)

    val drop : int -> t -> t
    (** [drop n s] is the slice made of the bytes of [s] without its
        first [n] bytes. This is {!eod} if [n > length s] and [s] itself
        if [n < 0] *)

    val break : int -> t -> t * t
    (** [break n s] is [take n s, drop n s]. *)

    (** {1:converting Converting} *)

    val of_bytes : ?first:int -> ?last:int -> bytes -> t
    (** [of_bytes ~first ~last b] is the slice made of the consecutive
        bytes of [b] whose indices exist in the non-empty
        range \[[first];[last]\].

        [first] defaults to [0] and [last] to [Bytes.length s - 1].
        Note that both [first] and [last] can be any integer. If
        [b] is empty or if [first > last] the interval is empty and
        [Invalid_argument] is raised. *)

    val of_bytes_or_eod : ?first:int -> ?last:int -> bytes -> t
    (** [of_bytes_or_eod] is like {!of_bytes} except that if the bytes
        are empty or if [first > last], {!eod} is returned. *)

    val to_bytes : t -> bytes
    (** [to_bytes t] copies the range of [s] to a new [bytes] value. *)

    val to_string : t -> string
    (** [to_string s] copies the range of [s] as a [string] value. *)

    val add_to_buffer : Buffer.t -> t -> unit
    (** [add_to_buffer b s] adds the byte range of [s] to [b]. *)

    val output_to_out_channel : Out_channel.t -> t -> unit
    (** [output_to_out_channel oc s] outputs the byte range of [s] to
        [oc]. {b Warning.} Make sure the channel is in
        {{!Out_channel.set_binary_mode}binary mode}, e.g. [stdout] is
        not by default. *)

    (** {1:format Formatting and inspecting} *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats a slice for inspection. This formats
        the range specification and at most the first four bytes of
        the buffer in hex. *)

    val pp' : ?head:int -> ?hex:bool -> unit -> Format.formatter -> t -> unit
    (** [pp'] is like {!pp} but prints raw bytes if [hex] is [false]
        (defaults to [true]) and prints at most [head] initial bytes
        (defaults to [4], use (-1) to format all the bytes). *)

    val tracer :
      ?pp:(Format.formatter -> t -> unit) ->
      ?ppf:Format.formatter -> id:string -> t -> unit
    (** [tracer ~pp ~ppf ~id] is a function that formats slices on
        [ppf] (defaults to {!Format.err_formatter}) with [pp]
        (defaults to {!pp)) and the identifier [id]. Use with
        {!Reader.trace_reads} or {!Writer.trace_writes}. *)

    (** {1:other Other} *)

    val io_buffer_size : int
    (** [io_buffer_size] is [65536] it should correspond to the value of
        OCaml's IO_BUFFER_SIZE. See
        {{:https://github.com/ocaml/ocaml/issues/5938}here}. *)

    val unix_io_buffer_size : int
    (** [unix_io_buffer_size] is [65536] it should correspond to the
        value of OCaml's UNIX_BUFFER_SIZE. See
        {{:https://github.com/ocaml/ocaml/issues/5938}here}. *)

    val check_length : int -> int
    (** [check_length l] is [l] if [l > 0] and raises
        [Invalid_argument] otherwise. *)
  end

  (** Byte stream readers.

      Byte streams are sequences of {{!Slice}bytes slices}. A byte stream
      reader provides read access to these slices in order but only
      slice by slice: the slice you get is {{!Slice.validity}valid for
      reading} only until the next slice is {{!Reader.read}read} from
      the reader. *)
  module Reader : sig

    (** {1:readers Byte stream readers} *)

    type t
    (** The type for byte stream readers. *)

    val make : ?stream_offset:int -> ?slice_length:int -> (unit -> Slice.t) -> t
    (** [make read] is a reader from the function [read] which enumerates
        the slices of a byte stream. The [read] function must satisfy the
        following properties:

        {ul
        {- [read ()] returns {!Slice.t} values until it returns
           a {!Slice.eod}. Once {!Slice.eod} is returned, further calls to
           [read] always return {!Slice.eod}}
        {- Returned slices must remain {{!Slice.validity}valid for reading}
           until the next call to [read ()].}}

        If [slice_length] is given, it provides a hint on the maximal
        length of slices that [read] returns. The zero-based
        {!stream_offset} is informational, defaults to [0]. *)

    val of_reader :
      ?stream_offset:int -> ?slice_length:int -> t -> (unit -> Slice.t) -> t
    (** [of_reader ?stream_offset ?slice_length r read] is like {!make}
        but unspecified parameters are copied from [r]. Not mandatory
        but implicitely it is assumed that [read] will read from [r]. *)

    val empty : t
    (** [empty] reads an empty byte stream. *)

    (** {1:props Properties} *)

    val slice_length : t -> int option
    (** [slice_length r] is a hint on the maximal length of
        slices that [f] generates (if any). *)

    val stream_offset : t -> int
    (** [stream_offset r] is the stream offset of [r]. This indicates
        that [r] enumerates the portion of a larger byte stream that
        starts at this offset. As far as byte readers are concerned it
        is purely informational. It can be useful for error
        reporting. *)

    val read_length : t -> int
    (** [read_length r] is the number of bytes returned by calls to
        {!read}. *)

    (** {1:reads Reading} *)

    val read : t -> Slice.t
    (** [read r] reads the next slice from [r]. The slice is {{!Slice.validity}
        valid for reading} until the next call to [read] on [r].
        Once {!Slice.eod} is returned, {!Slice.eod} is always returned. *)

    (** {1:convert Converting} *)

    val of_bytes : ?stream_offset:int -> ?slice_length:int -> bytes -> t
    (** [of_bytes s] reads the bytes of [b] with slices of maximal
        length [slice_length] (defaults to [Bytes.length s]). [stream_offset]
        is given to {!make}. *)

    val of_string : ?stream_offset:int -> ?slice_length:int -> string -> t
    (** [of_string b] is like {!of_bytes} but reads the bytes of [s]. *)

    val of_in_channel :
      ?stream_offset:int -> ?slice_length:int -> In_channel.t -> t
    (** [of_in_channel ic], sets [ic] in
        {{!In_channel.set_binary_mode}binary mode} and reads the bytes
        of [ic] with slices of maximal length [slice_length] (defaults
        to {!Slice.io_buffer_size}). [stream_offset] is given to
        {!make}. *)

    val to_string : t -> string
    (** [to_string r] reads [r] until {!Slice.eod} into a string [s]. *)

    val add_to_buffer : Buffer.t -> t -> unit
    (** [add_to_buffer b r] reads [r] and adds its slices to [b] until
        {!Slice.eod}. *)

    val output_to_out_channel : ?flush_slices:bool -> Out_channel.t -> t -> unit
    (** [output_to_out_channel oc r], sets [oc] in
        {{!Out_channel.set_binary_mode}binary mode}, reads [r] and
        outputs the slices on [oc] until {!Slice.eod}. If
        [flush_slices] is [true], [oc] is
        {{!Out_channel.flush}flushed} after each slice except
        {!Slice.eod}. *)

    (** {1:other Other} *)

    val trace_reads : (Slice.t -> unit) -> t -> t
    (** [trace_reads f w] invokes [f] with the slice before
        returning them with {!read}. *)

    val get_stream_offset : none:t -> int option -> int
    (** [get_stream_offset ~none o] is the stream offset of [none]
        if [o] is [None] and [i] if [o] is [Some i]. *)
  end

  (** Byte stream writers.

      Byte streams are sequences of {{!Slice}bytes slices}. A byte
      stream writer is given access to these slices in order but only
      slice by slice in its slice iteration function: slices only
      remain {{!Slice.validity}valid for reading} until the function
      returns. *)
  module Writer : sig

    type t
    (** The type for byte stream writers. *)

    val make : ?stream_offset:int -> ?slice_length:int -> (Slice.t -> unit) -> t
    (** [make write] is a writer from the function [write] which
        iterates over the slices of a byte stream. The [write]
        function is called as follows:

        {ul
        {- [write] is called with {!Slice.t} values until it is called
           with a {!Slice.eod} value. Once [write] was called with {!Slice.eod}
           it will only ever be called with {!Slice.eod}.}
        {- Slice values given to [write] must remain {{!Slice.validity}valid
           for reading} until the [write] function returns.}}

        If [slice_length] is given, it provides a hint on the maximal
        length of slices that [write] would like to receive. The
        zero-based [stream_offset] is informational, it can be used to
        indicate that [write] iterates over a portion of a larger byte
        stream that starts at [stream_offset] (defaults to [0]). *)

    val of_writer :
      ?stream_offset:int -> ?slice_length:int -> t -> (Slice.t -> unit) -> t
    (** [of_writer ?stream_offset ?slice_length w write] is like {!make}
        but unspecified parameters are copied from [w]. Not mandatory
        but implicitely it is assumed that [write] will write to [w]. *)

    (** {1:properties Properties} *)

    val slice_length : t -> int option
    (** [slice_length w] is a hint on the maximal length of slices that
        [w] would like to receive (if any). *)

    val stream_offset : t -> int
    (** [stream_offset w] is the stream offset of [w]. This indicates
        that [w] iterates a portion of a larger byte stream that
        starts at this offset. As far as byte writers are concerned
        it is purely informational. It can be useful for error
        reporting. *)

    val written_length : t -> int
    (** [written_length w] is the number of bytes written on [w]. *)

    (** {1:writing Writing} *)

    val write : t -> Slice.t -> unit
    (** [write w s] writes the slice [s] on [w]. [s] must remain
        {{!Slice.validity}valid for reading} until the function
        returns. *)

    val write_eod : t -> unit
    (** [write_eod w] is [write w Slice.eod]. *)

    val write_bytes : t -> bytes -> unit
    (** [write_bytes w b] writes the bytes [b] on [w] in
        {!slice_length} slices or a single slice if [None]. The bytes
        of [b] must not change until the function returns. *)

    val write_string : t -> string -> unit
    (** [write_string] is like {!write_bytes} but writes a string. *)

    val write_reader : eod:bool -> t -> Reader.t -> unit
    (** [write_reader w r] writes the slices of [r]
        on [w]. {!Slice.eod} is only written if [eod] is [true].
        Note that the slices are written as given by [r] and may
        not respect [w]'s desired {!slice_length}. *)

    val write_in_channel : eod:bool -> t -> In_channel.t -> unit
    (** [write_in_channel w ic] sets [ic] to
        {{!In_channel.set_binary_mode}binary mode} and writes slices
        to [w] until the end of file is reached at which point
        {!Slice.eod} is written iff [eod] is true. The maximal length
        of written slices are [w]'s {!slice_length}. *)

   (** {1:convert Converting} *)

    val of_out_channel :
      ?stream_offset:int -> ?slice_length:int -> ?flush_slices:bool ->
      Out_channel.t -> t
    (** [of_out_channel oc] sets [oc] to binary mode and writes slices
        to [oc]. If [flush_slices] is [true] (defaults to [false]), [oc] is
        {{!Out_channel.flush}flushed} after each slice except {!Slice.eod}.

        [slice_length] and [stream_offset] are given to {!make}.
        [slice_length] defaults {!Slice.io_buffer_size}. *)

    val of_buffer : ?stream_offset:int -> ?slice_length:int -> Buffer.t -> t
    (** [of_buffer b] writes slices to [b].

        [slice_length] and [stream_offset] are given to {!make}.
        [slice_length] is not that important but we default it to
        {!Slice.io_buffer_size}, so that channel and fd readers that
        may ajust their own buffers on writers adjust nicely for
        themselves. *)

    (** {1:other Other} *)

    val trace_writes : (Slice.t -> unit) -> t -> t
    (** [trace_writes f w] invokes [f] with the slice before
        giving them to [w]. *)

    val get_stream_offset : none:t -> int option -> int
    (** [get_stream_offset ~none o] is the stream offset of [none]
        if [o] is [None] and [i] if [o] is [Some i]. *)
  end

  (** {1:bytes Formatters} *)

  val pp_hex :
    ?addr:bool -> ?addr_start:int -> ?addr_div:int -> ?count:int ->
    ?group:int -> ?ascii:bool -> ?start:int -> ?len:int -> unit ->
    Format.formatter -> bytes -> unit
  (** [pp_hex ~addr ~addr_start ~count ~group ~ascii ~start ~len:n () ppf
      b] prints the bytes in range \[[start];[start + n - 1]\] in
      hexadecimal. [start] defaults to [0] and [len] to [length b -
      start]. Formats nothing if [len] is [0].
      The formatting options are as follows:
      {ul
      {- If [addr] is [true] (defaults to [false]), starts each line
       with the index of the first byte on the line as a 32-bit or 64-bit
       hexadecimal number (adjusted according the values of [addr_start] and
       [len]). Finishes the output with a blank line that has the address
       following the last byte.}
      {- If [addr_start] is specified uses this as the start index for
       the first byte (defaults to [start])}
      {- If [addr_div] is specified addresses are divided by this number.
       Defaults to [1]. For example using [4] indexes by 32-bits.}
      {- [count] defines the number of bytes, printed on each line. Defaults to
       [16].}
      {- [group] is the number of bytes that are grouped together. Defaults to
       [2], i.e. shows hexadecimal 16-bit numbers.}
      {- If [ascii] is [true] (defaults to [false]) ends each line with
       a column with the bytes interpreted as US-ASCII.}} *)
end
