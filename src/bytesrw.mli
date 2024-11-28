(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Extended {!Stdlib.Bytes} module. Open to use it.

    Open the module to use it. This adds these modules:
    {!modules:
    Bytes.Slice
    Bytes.Stream
    Bytes.Reader
    Bytes.Writer}
    to the {!Stdlib.Bytes} module. *)

(** Extended {!Stdlib.Bytes} module. *)
module Bytes : sig

  include module type of Stdlib.Bytes (** @closed *)

  (** {1:streams Byte streams} *)

  (** Byte slices.

      A byte slice is a {e non-empty} consecutive range of bytes in a
      {!Bytes.t} value. The unique, distinguished, {e empty} slice {!Slice.eod}
      is used to indicate end of data. *)
  module Slice : sig

    (** {1:validity Validity}

        The bytes in the range of a slice is made available to a third-party
        for a limited amount of time during which the slice is said to be {e
        valid} for reading or writing (or both). Third parties are
        only allowed to access the bytes in the range and in the mode
        specified while it is valid. *)

    (** {1:slice Slice lengths} *)

    type length = int
    (** The type for slice lengths. A {e positive} integer. *)

    val default_length : length
    (** [default_length] is {!io_buffer_size}. *)

    val io_buffer_size : length
    (** [io_buffer_size] is [65536] it should correspond to the value of
        OCaml's IO_BUFFER_SIZE. See
        {{:https://github.com/ocaml/ocaml/issues/5938}here}. *)

    val unix_io_buffer_size : length
    (** [unix_io_buffer_size] is [65536] it should correspond to the
        value of OCaml's UNIX_BUFFER_SIZE. See
        {{:https://github.com/ocaml/ocaml/issues/5938}here}. *)

    val check_length : int -> length
    (** [check_length l] is [l] if [l > 0] and raises
        [Invalid_argument] otherwise. *)

    (** {1:slices Slices} *)

    type t
    (** The type for byte slices. *)

    val make : bytes -> first:int -> length:length -> t
    (** [make b ~first ~length] is a slice referencing the bytes of [b] in
        the range \[[first]; [first+length-1]\].

        This function does not allow the creation of the empty
        {!Slice.eod} which is a feature. It raises [Invalid_argument]
        if [length] is not positive, larger than the length of [b] or
        if [first] is out of bounds.

        See also {!make_or_eod} and {!of_bytes}. *)

    val make_or_eod : bytes -> first:int -> length:int -> t
    (** [make_or_eod] is like {!make} but returns {!eod} instead of
        raising [Invalid_argument] if [length] is zero. *)

    val bytes : t -> bytes
    (** [bytes s] are the underlying bytes of the slice [s]. *)

    val first : t -> int
    (** [first s] is the index, in [bytes s], of the first byte of the byte
        range of [s]. *)

    val length : t -> int
    (** [length s] is the byte length of the byte range of [s]. This returns [0]
        only on {!eod}. *)

    val copy : tight:bool -> t -> t
    (** [copy ~tight s] is a copy of [s]. If [tight] is [true], the
        copy contains only the bytes in the range of [s]. If not the
        whole [bytes s] is copied. *)

    (** {1:end_of_data End of data} *)

    val eod : t
    (** [eod] is a slice to denote the end of data. It is the only slice
        with [length d = 0]. Its bytes are {!Bytes.empty}. *)

    val is_eod : t -> bool
    (** [is_eod s] is [true] iff [s == eod]. *)

    (** {1:predicates Predicates and comparisons} *)

    val equal : t -> t -> bool
    (** [equal s0 s1] is [true] iff the bytes in the slice ranges of [s0]
        and [s1] are equal. *)

    val compare : t -> t -> int
    (** [compare s0 s1] sorts the bytes in the slice ranges of [s0] and
        [s1] in lexicographic order. *)

    (** {1:breaking Breaking slices}

        {b Warning.} In these operations index specification are in {e
        slice space} which starts at 0 at the slice's {!first}
        byte. *)

    val take : int -> t -> t option
    (** [take n s] is the slice made of the first [n] bytes starting
        at {!first}. This is [None] if the operation results in
        {!eod}, including if [s] is {!eod} or if [n < 0]. *)

    val drop : int -> t -> t option
    (** [drop n s] is the slice made of the bytes in the range of [s]
        without the first [n] bytes starting at {!first}. This is
        [None] if the operation results in {!eod}, including if [s] is
        {!eod} or if [n < 0]. *)

    val break : int -> t -> (t * t) option
    (** [break n s] is [(take n s, drop n s)] but returns [None]
        if any result is [None]. *)

    val sub : t -> first:int -> length:int -> t
    (** [sub s ~first ~length] is the slice made of the consecutive bytes
        of the range [b] whose indices exist in the non-empty
        slice space range \[[first];[first + length - 1]\]. Raises
        [Invalid_argument] if the interval is empty or out of bounds.
        See also {!sub_or_eod}. *)

    val sub_or_eod : t -> first:int -> length:int -> t
    (** [sub_or_eod] is like {!sub} except that if the interval is
        empty, {!eod} is returned. *)

    val subrange : ?first:int -> ?last:int -> t -> t
    (** [subrange ~first ~last s] is the slice made of the consecutive
        bytes of the range of [s] whose indices exist in the non-empty
        slice space range \[[first];[last]\].

        [first] defaults to [0] and [last] to [Slice.length s - 1].
        Note that both [first] and [last] can be any integer. If
        [s] is {!eod} or if [first > last] the interval is empty
        and [Invalid_argument] is raised. See also {!subrange_or_eod} and
       {!make}. *)

    val subrange_or_eod : ?first:int -> ?last:int -> t -> t
    (** [subrange_or_eod] is like {!of_bytes} except that if
        the bytes are empty or if [first > last], {!eod} is returned. *)

    (** {1:converting Converting} *)

    val of_bytes : ?first:int -> ?last:int -> bytes -> t
    (** [of_bytes ~first ~last b] is the slice made of the consecutive
        bytes of [b] whose indices exist in the non-empty
        range \[[first];[last]\]. The bytes are not copied.

        [first] defaults to [0] and [last] to [Bytes.length s - 1].
        Note that both [first] and [last] can be any integer. If
        [b] is empty or if [first > last] the interval is empty and
        [Invalid_argument] is raised. See also {!of_bytes_or_eod}. *)

    val of_bytes_or_eod : ?first:int -> ?last:int -> bytes -> t
    (** [of_bytes_or_eod] is like {!of_bytes} except that if the bytes
        are empty or if [first > last], {!eod} is returned. *)

    val of_bigbytes :
      ?first:int -> ?last:int ->
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
      t
    (** [of_bigbytes] is like {!of_bytes} but {b copies} data from a bigbytes
        value. *)

    val of_bigbytes_or_eod : ?first:int -> ?last:int ->
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
      t
    (** [of_bytes_or_eod] is like {!of_bytes_or_eod} but {b copies} data from
        a bigbytes value. *)

    val of_string : ?first:int -> ?last:int -> string -> t
    (** [of_string s] is [of_bytes (Bytes.of_string s)]. *)

    val of_string_or_eod : ?first:int -> ?last:int -> string -> t
    (** [of_string_or_eod] is [of_bytes_or_eod (Bytes.of_string s)]. *)

    val to_bytes : t -> bytes
    (** [to_bytes t] copies the range of [s] to a new [bytes] value. *)

    val to_bigbytes : t ->
      (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
    (** [to_bigbytes t] copies the range of [s] to a new bigbytes value. *)

    val to_string : t -> string
    (** [to_string s] copies the range of [s] to a new [string] value. *)

    val add_to_buffer : Buffer.t -> t -> unit
    (** [add_to_buffer b s] adds the byte range of [s] to [b]. *)

    val output_to_out_channel : Out_channel.t -> t -> unit
    (** [output_to_out_channel oc s] outputs the byte range of [s] to
        [oc]. {b Warning.} Make sure the channel is in
        {{!Out_channel.set_binary_mode}binary mode}. For example by
        default [stdout] is not. *)

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
        (defaults to {!pp}) and the identifier [id]. Use with
        {!Reader.tap} or {!Writer.tap}. *)
  end

  (** Byte streams.

      Byte streams are sequences of non-empty {{!Slice}byte slices}
      ended by a single {!Slice.eod} slice. They have no concrete
      incarnation, they are observed by {!Bytes.Reader}s and
      {!Bytes.Writer}s.

      See the {{!page-index.quick}quick start} and read more
      about streams in the {{!page-tutorial.streams}tutorial}. *)
  module Stream : sig

    (** {1:positions Positions} *)

    type pos = int
    (** The type for stream positions.

        The position of a stream is the zero-based byte index of
        the next byte to read or write. It can also be seen as the
        count of bytes read or written by a stream reader or
        writer. *)

    (** {1:formats Formats} *)

    type format = string
    (** The type for stream formats. An identifier for the format of
        read or written bytes. Favour mime types or lowercased file extensions
        without the dot. *)

    (** {1:errors Errors}

        Read more about errors in the {{!page-tutorial.errors}tutorial}. *)

    type error = ..
    (** The type for stream errors. Stream formats add their own cases
        to this type. See {{!page-cookbook.errors}an example}. *)

    type error_context
    (** The type for error contexts. *)

    exception Error of (error * error_context)
    (** The exception raised by streams reader, writers and their
        creation functions. *)

    val error_message : error * error_context -> string
    (** [error_message e] turns error [e] into an error message for humans. *)

    val error_to_result : error * error_context -> ('a, string) result
    (** [error_to_result e] is [Result.Error (error_message e)]. *)

    type 'e format_error
    (** The type for describing errors of type ['e] for a stream format. *)

    val make_format_error :
      format:format -> case:('e -> error) -> message:(error -> string) ->
      'e format_error
    (** [make_format_error ~format ~case ~message] describes the type
        of error for format [format]:
        {ul
        {- [format] identifies the stream {!format}.}
        {- [case] is the function that injects the type into {!type-error}.}
        {- [message] is a function that must stringify the results of [case].}}

        See {{!page-cookbook.errors}an example}. *)

    val error : 'e format_error -> ?context:[`R| `W] -> 'e -> 'a
    (** [error fmt e] errors with [e] for a stream [fmt] by raising an
        {!Error} exception. *)

    (** {1:limits Limits} *)

    type error += Limit of int (** The inclusive byte count limit. *)
    (** The type for stream limits errors. *)

    val limit_error : int format_error
    (** [limit_error] is a stream limit error. *)
  end

  (** Byte stream readers.

      Byte streams are sequences of non-empty {{!Slice}byte slices}
      ended by a single {!Slice.eod} slice. A byte stream reader
      provides read access to these slices in order, on demand, but
      only slice by slice: the slice you get is
      {{!Slice.validity}valid for reading} only until the next slice
      is {{!Reader.read}read} from the reader.

      See the {{!page-index.quick}quick start} and read about
      stream readers in the {{!page-tutorial.readers}tutorial}. *)
  module Reader : sig

    (** {1:readers Readers} *)

    type t
    (** The type for byte stream readers. *)

    val make :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> (unit -> Slice.t) -> t
    (** [make read] is a reader from the function [read] which
        enumerates the slices of a byte stream. The contract
        between the reader and [read] is as follows:
        {ul
        {- The slice returned by a call to [read] must remain
           {{!Slice.validity}valid for reading} until the next call to [read].}
        {- The reader guarantees to dereference [read] and never call it
           again as soon as {!Slice.eod} is returned by [read].}}
        [pos] defaults to [0] and [slice_length] to {!Slice.default_length}. *)

    val empty : ?pos:Stream.pos -> ?slice_length:Slice.length -> unit -> t
    (** [empty ()] is [make (Fun.const Slice.eod)], an empty byte stream. *)

    val pos : t -> Stream.pos
    (** [pos r] is the {{!Stream.pos}stream position} of the next byte to
        read. Alternatively it can be seen as the number of bytes
        returned by calls to [read] (not including {{!push_back}push back}
        replays), see {!read_length}.

        {b Warning.} Due to {{!push_back}push backs} negative values can be
        returned. *)

    val read_length : t -> int
    (** [read_length r] is an alternative name for {!pos}.

        {b Warning.} Due to {{!push_back}push backs} negative values can be
        returned. *)

    val slice_length : t -> Slice.length
    (** [slice_length r] is a hint on the maximal length of slices
        that [r] returns. *)

    val error : 'e Stream.format_error -> t -> ?pos:Stream.pos -> 'e -> 'a
    (** [error fmt r e] raises a {!Stream.Error} [e] of format
        [fmt] for [r]. [pos] is the position reported for the error, it
        defaults to [r]'s {!pos}. If the value is negative it is
        added to the latter, e.g. using the negated length of
        the last slice would report an error at the first byte of the
        last slice.  *)

    (** {1:reads Reading} *)

    val read : t -> Slice.t
    (** [read r] reads the next slice from [r]. The slice is only
        {{!Slice.validity} valid for reading} until the next call to
        [read] on [r]. Once {!Slice.eod} is returned, {!Slice.eod} is
        always returned. *)

    val push_back : t -> Slice.t -> unit
    (** [push_back r s] pushes the slice [s] back on [r]. If [s]
        is {!Slice.eod} this has no effect. Otherwise the stream position is
        rewinded by [Slice.length s] and the next {!read} on [r] returns [s].

        {b Note.} If [r] is the result of a call to {!tap} the tap
        function won't see the push backs. Good for your checksums.

        {b Warning.} This should not be used as a general lookahead
        mecanism by stream readers. Codecs should devise their own
        buffering structures. But it is useful for stream content
        {{!sniff}sniffing} and breaking streams into substreams at
        precise positions.

        {b Warning.} Currently it is possible to push back
        beyond the beginning of a stream. It is unclear whether
        this feature will be kept in the future. Please get in touch
        if you use that feature. *)

    val sniff : int -> t -> string
    (** [sniff n r] sniffs at most [n] bytes from [r]. These bytes will still
        be returned by {!read} calls. Less than [n] bytes are returned
        if the end of stream is reached before or if [n <= 0].

        {b Warning.} This uses {!push_back} and should not be
        used as a general lookahead mecanism by stream readers. *)

    val skip : int -> t -> unit
    (** [skip n r] is skips {e at most} [n] bytes from [r]. See also
        {!discard}. *)

    val discard : t -> unit
    (** [discard r] reads and discards slices until {!Slice.eod} is returned.
        See also {!skip}. *)

    (** {1:filters Filters}

        Read more about filters in the
        {{!page-tutorial.reader_filters}tutorial}. *)

    type filter = ?pos:Stream.pos -> ?slice_length:Slice.length -> t -> t
    (** The type for byte stream reader filters.

        Given a reader [r], a filter [f] returns a filtering reader [f
        r], that reads the stream of [r] and transforms it in some
        way. The following conventions should be followed for the
        resulting filtering reader:

        {ul
        {- If [pos] is unspecified, it should default to [r]'s position
           only if the reads on the filtering reader are always in the same
           position space as [r] (see e.g. {!sub} or {!limit}). Otherwise
           it should be [0] (e.g. on decompression filters) or anything
           else that makes sense for the filter.}
        {- If [slice_length] is unspecified, it should default
           to [r]'s {!slice_length} or what makes more sense for the filter's
           reads.}
        {- If the filter reader does not read all of [r]'s bytes, it
           must, after having returned {!Slice.eod}, leave [r] at the
           position of the leftover data so that [r] can be used again
           to perform non-filtered reads. This can be done by using
           {!push_back} on [r] with the leftover data.}
        {- If your filter is in a module [M], then its name should be
           [M.{decode,encode}_reads] or another meaningful verb like
           [M.{decompress,compress}_reads],
           [M.{encrypt,decrypt}_reads], etc.}} *)

    val sub : int -> filter
    (** [sub n r] is a reader reading at most [n] bytes from [r]
        before returning {!Slice.eod}. [sub] satisfies all the
        {!type-filter} conventions and is not affected by push backs:
        reading back a push back on the sub stream does not count towards [n].
        See also {!limit}. *)

    val limit : ?action:(t -> int -> unit) -> int -> filter
    (** [limit n r] is like {!sub} except it invokes [action] {b once}
        with the filter reader before returning {!Slice.eod}. The
        default [action] raises {!Stream.Limit} error. See
        {{!page-cookbook.limiting}an example}. *)

    val filter_string : filter list -> string -> string
    (** [filter_string fs s] is a convenience function
        that applies the filters [fs], from left to right
        to the string [s] it is equivalent to:
        {[
          to_string (List.fold_left (fun r f -> f r) (of_string s) fs)
        ]}
        See an {{!page-cookbook.string_filtering}example}. *)

    val reslice : filter
    (** [reslice ?pos ?slice_length r] has the data of [r] but ensures
        that all slices are of length [slice_length] (defaults to
        {!slice_length}[ r]) except perhaps the last one. *)

    (** {1:append Appending} *)

    val append : ?pos:Stream.pos -> ?slice_length:Slice.length -> t -> t -> t
    (** [append r0 r1] reads from [r0] and then from [r1]. [pos]
        defaults to [0] and [slice_length] defaults to the maximal
        length of [r0] and [r1].  *)

    (** {1:taps Taps} *)

    val tap : (Slice.t -> unit) -> t -> t
    (** [tap f r] invokes [f] with the slice read by [r] before returning
        them with {!read}. Note that {!push_back}s are not tapped, so
        this can be used reliably for checksumming the reads of [r].
        See also {!Slice.tracer} and {{!page-cookbook.tracing}an example}. *)

    (** {1:predicates Predicates and comparisons} *)

    val equal : t -> t -> bool
    (** [equal r0 r1] asserts that the bytes of [r0] are equal to those
        of [r1] in bounded space. This may not fully consume the readers. *)

    val compare : t -> t -> int
    (** [equal r0 r1] sorts the bytes of [r0] and [r1] in lexicographic order
        in bounded space. This may not fully consume the readers. *)

    (** {1:convert Converting} *)

    val of_bytes :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> bytes -> t
    (** [of_bytes s] reads the bytes of [b] with slices of maximal
        length [slice_length] which defaults to [Bytes.length s].
        [pos] defaults to [0]. *)

    val of_string :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> string -> t
    (** [of_string b] is like {!of_bytes} but reads the bytes of [s]. *)

    val of_in_channel :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> In_channel.t -> t
    (** [of_in_channel ic], sets [ic] in
        {{!In_channel.set_binary_mode}binary mode} and reads the bytes
        of [ic] with slices of maximal length [slice_length] which
        defaults to {!Slice.io_buffer_size}. [pos] default to
        {!In_channel.pos}. This function and the resulting reader
        may raise [Sys_error].  *)

    val of_slice :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> Slice.t -> t
    (** [of_slice s] reads [r] with slice of maximal length
        [slice_length] which default to [Slice.length s]. [pos]
        defaults to [0]. *)

    val of_slice_seq :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> Slice.t Seq.t -> t
    (** [of_slice_seq seq] reads the slices produced by [seq]. [pos]
        defaults to [0] and [slice_length] defaults to [None]. *)

    val to_string : t -> string
    (** [to_string r] reads [r] until {!Slice.eod} into a string [s]. *)

    val to_slice_seq : t -> Slice.t Seq.t
    (** [to_slice_seq r] reads [r] until {!Slice.eod} into a sequence.
        The latter is not enumerated.

        {b Warning.} A slice returned by the sequence is only
        {{!Slice.validity}valid for reading} until the next slice is
        requested from the sequence. *)

    val add_to_buffer : Buffer.t -> t -> unit
    (** [add_to_buffer b r] reads [r] and adds its slices to [b] until
        {!Slice.eod}. *)

    val output_to_out_channel : ?flush_slices:bool -> Out_channel.t -> t -> unit
    (** [output_to_out_channel oc r], sets [oc] in
        {{!Out_channel.set_binary_mode}binary mode}, reads [r] and
        outputs the slices on [oc] until {!Slice.eod}. If
        [flush_slices] is [true], [oc] is
        {{!Out_channel.flush}flushed} after each slice except
        {!Slice.eod}. May raise [Sys_error]. *)

    (** {1:fmt Formatting} *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats a readers's properties for inspection. It does
        not consume any input. *)
  end

  (** Byte stream writers.

      Byte streams are sequences of non-empty {{!Slice}byte slices}
      ended by a single {!Slice.eod} slice. A byte stream writer is
      given access to these slices in order but only slice by slice in
      its write function: the slice only remains
      {{!Slice.validity}valid for reading} until the write function
      returns.

      See the {{!page-index.quick}quick start} and read more about
      stream writers in the {{!page-tutorial.writers}tutorial}. *)
  module Writer : sig

    (** {1:writers Writers} *)

    type t
    (** The type for byte stream writers. *)

    val make :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> (Slice.t -> unit) -> t
    (** [make write] is a writer from the function [write] which
        iterates over the slices of a byte stream. The contract
        between the writer and [write] is as follows:

        {ul
        {- The slice values given to [write] are {{!Slice.validity}valid
           for reading} only until the [write] function returns.}
        {- The writer guarantees that after [write] was called with
           {!Slice.eod}, [write] is dereferenced by the writer and
           will never be called again.}}

        [pos] defaults to [0] and slice_length to {!Slice.default_length}. *)

    val ignore : ?pos:Stream.pos -> ?slice_length:Slice.length -> unit -> t
    (** [ignore ()] is [make (fun _ -> ())], a writer that ignores the
        writes that are pushed on it. *)

    val pos : t -> Stream.pos
    (** [pos w] is the {{!Stream.pos}stream position} of the next byte
        to write. Alternatively it can be seen as the number of bytes
        written on [w], see {!written_length}. *)

    val slice_length : t -> Slice.length
    (** [slice_length w] is a hint on the maximal length of slices that
        [w] would like to receive. *)

    val written_length : t -> int
    (** [written_length w] is an alternative name for {!pos}. *)

    val error : 'e Stream.format_error -> t -> ?pos:Stream.pos -> 'e -> 'a
    (** [error fmt w e] raises a {!Stream.Error} [e] of format [fmt]
        for [w]. [pos] is the position reported for the error it
        defaults to [w]'s {!pos}. If the value is negative it is added
        to the latter, e.g. using the negated length of the last slice
        would report an error at the first byte of the last slice. *)

    (** {1:writing Writing}

        {b Note.} All these functions raise [Invalid_argument]
        if a slice other than {!Slice.eod} is written after
        a {!Slice.eod} was written. *)

    val write : t -> Slice.t -> unit
    (** [write w s] writes the slice [s] on [w]. The slice [s] must remain
        {{!Slice.validity}valid for reading} until the function
        returns.

        The function raises [Invalid_argument] is [s] is written
        after a {!Slice.eod} and [s] is not {!Slice.eod}. *)

    val write_eod : t -> unit
    (** [write_eod w] is [write w Slice.eod]. Only {!Slice.eod}
        can be written on [w] aftewards. *)

    val write_bytes : t -> bytes -> unit
    (** [write_bytes w b] writes the bytes [b] on [w] in
        {!slice_length} slices, except perhaps the last one. The bytes
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

    (** {1:filters Filters}

        Read more about filters in the
        {{!page-tutorial.writer_filters}tutorial}. *)

    type filter =
      ?pos:Stream.pos -> ?slice_length:Slice.length -> eod:bool -> t -> t
    (** The type for byte stream writer filters.

        Given a writer [w], a filter [f] returns a filtering writer [f
        ~eod w], that transforms the writes made on it in some way and
        then writes them to [w]. The following conventions should be
        followed for the filtering writer:

        {ul
        {- Once the filter writer receives {!Slice.eod}, it must
           stop and, if applicable, report an error if there was leftover
           data that it couldn't write. It must only write {!Slice.eod}
           on [w] if [eod] is [true]. Otherwise it should leave [w] as
           is so that [w] can be used again to perform other
           non-filtered writes.}
        {- If [pos] is unspecified it should default to [w]'s position
           only if the writes are in the same position space as [w].
           Otherwise it should be [0] (e.g. on compression filters).}
        {- If [slice_length] is unspecified it should default to [w]'s
           value or a value that makes more sense for the filter's needs.}
        {- The filtering writer should write slices on [w] that respects
           its [slice_length] desires.}
        {- If your filter is in a module [M], then its name should be
           [M.{decode,encode}_writes] or an other meaningful verb like
           [M.{decompress,compress}_writes], [M.{encrypt,decrypt}_writes],
           etc.}} *)

    val limit : ?action:(t -> int -> unit) -> int -> filter
    (** [limit n w ~eod] is a writer that writes {e at most} [n] bytes on [w].
        Any leftover in the slice that exceeds the [n] bytes is
        lost. If the [n] bytes are exceeded, {!Slice.eod} is written
        if [eod] is [true], the action
        [action] is invoked once and the filtering writer accepts only
        {!Slice.eod} afterwards. As per filter semantics {!Slice.eod}
        is only written on [w] if [eod] is true. The default [action]
        raises {!Stream.Limit} error. *)

    val filter_string : filter list -> string -> string
    (** [filter_string fs s] is a convenience function
        that applies the filters [fs], from left to right
        to the string [s] it is equivalent to:
        {[
          let b = Buffer.create (String.length s) in
          let w = List.fold_left (fun w f -> f ~eod:true w) (of_buffer w) fs in
          write_string w s; write_eod w; Buffer.contents b
        ]}
        See an {{!page-cookbook.string_filtering}example}. *)

    (** {1:taps Taps} *)

    val tap : (Slice.t -> unit) -> t -> t
    (** [tap f w] is a writer that invokes [f] with the slice before writing
        it on [w]. See also {!Slice.tracer} and
        {{!page-cookbook.tracing}an example}.  *)

    (** {1:convert Converting} *)

    val of_out_channel :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> ?flush_slices:bool ->
      Out_channel.t -> t
    (** [of_out_channel oc] sets [oc] to binary mode and writes slices
        to [oc]. If [flush_slices] is [true] (defaults to [false]), [oc] is
        {{!Out_channel.flush}flushed} after each slice except {!Slice.eod}.
        [pos] defaults to {!Out_channel.pos}. The hinted
        {!slice_length} defaults to {!Slice.io_buffer_size}. This function
        and the resulting writer may raise [Sys_error]. *)

    val of_buffer :
      ?pos:Stream.pos -> ?slice_length:Slice.length -> Buffer.t -> t
    (** [of_buffer b] writes slices to [b].

        [slice_length] is not that important but we default it to
        {!Slice.io_buffer_size}, so that channel and fd readers that
        may ajust their own buffers on writers adjust nicely for
        themselves. *)

    (** {1:fmt Formatting and inspecting} *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats a writer's properties for inspection. *)
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
