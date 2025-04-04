(*---------------------------------------------------------------------------
   Copyright (c) 2025 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw

let strf = Printf.sprintf
let char_ascii_lower_hex_digit_of_int n = (* available in OCaml 5.4 *)
  let d = abs (n mod 16) in
  Char.unsafe_chr (if d < 10 then 0x30 + d else 0x57 + d)

module Psa = Bytesrw_crypto__psa

(* Preliminaries *)

type uint8 = int
(** The type for unsigned 8-bit integers. *)

type uint16 = int
(** The type for unsigned 16-bit integers. *)

type uint32 = int32
(** The type for unsigned 32-bit integers. *)

type bigbytes =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Bigbytes = struct
  type t = bigbytes
  let create n = Bigarray.(Array1.create int8_unsigned c_layout n)
  let make n ~fill = let a = create n in Bigarray.Array1.fill a fill; a
  let init n init = Bigarray.(Array1.init int8_unsigned c_layout n init)
  let length b = Bigarray.Array1.dim b
  let[@inline] get b i = (Bigarray.Array1.get : t -> int -> int) b i
  let[@inline] get_char b i = Char.chr (get b i)
  let copy b = let b' = create (length b) in Bigarray.Array1.blit b b'; b'
  let clear b = Bigarray.Array1.fill b 0

  let get_length len b =
    let blen = length b in
    match len with
    | None -> blen
    | Some length ->
        if 0 <= length && length <= blen then length else
        invalid_arg (strf "length not in range [0;%d]" blen)

  (* XXX a few of these operations could be made faster with memcpy here. *)
  let of_string s = init (String.length s) (String.get_uint8 s)
  let to_string ?length b = String.init (get_length length b) (get_char b)
  let of_bytes b = init (Bytes.length b) (Bytes.get_uint8 b)
  let to_bytes ?length b = Bytes.init (get_length length b) (get_char b)

  let pp ppf b =
    let max = length b - 1 in
    if max < 0 then () else
    let byte = get b 0 in
    Format.pp_print_char ppf (char_ascii_lower_hex_digit_of_int (byte lsr 4));
    Format.pp_print_char ppf (char_ascii_lower_hex_digit_of_int byte);
    for i = 1 to max do
      let byte = get b i in
      Format.pp_print_cut ppf ();
      Format.pp_print_char ppf (char_ascii_lower_hex_digit_of_int (byte lsr 4));
      Format.pp_print_char ppf (char_ascii_lower_hex_digit_of_int byte);
    done
end

(* Errors *)

exception Panic of string

let panic_printer = function
| Panic e -> Some (strf "Bytesrw_crypto.Panic <%s>" e)
| _ -> None

let err ctx rc = strf "%s: %s" ctx (Psa.Status.message rc)
let err_init rc = err "psa_crypto_init" rc
let err_rand rc = err "psa_generate_random" rc

let panicf fmt = Format.kasprintf (fun e -> raise (Panic e)) fmt
let panic_status ctx st = raise (Panic (err ctx st))

type Bytes.Stream.error += Error of string

let format_error =
  let case msg = Error msg in
  let message = function Error msg -> msg | _ -> assert false in
  Bytes.Stream.make_format_error ~format:"crypto" ~case ~message

let stream_error e = Bytes.Stream.error format_error e
let reader_error r e = Bytes.Reader.error format_error r e
let writer_error w e = Bytes.Writer.error format_error w e

(* Module toplevel initialisation. Install a printer for Panic
   and automatically initialize the PSA Crypto API *)

let init () =
  Printexc.register_printer panic_printer;
  match Psa.crypto_init () with 0 -> () | rc -> raise (Panic (err_init rc))

let () = init ()

(* Cryptography *)

module Clear = struct
  let slice s =
    let bytes = Bytes.Slice.bytes s in
    let first = Bytes.Slice.first s in
    let last = first + Bytes.Slice.length s - 1 in
    for i = first to last do Bytes.set bytes i '\x00' done

  let bytes b = Bytes.fill b 0 (Bytes.length b) '\x00'
  let bigbytes = Bigbytes.clear
end

module Verify = struct
  external equal_slices : Bytes.Slice.t -> Bytes.Slice.t -> bool =
    "ocaml_bytesrw_crypto_verify_equal_slices"

  let equal_strings s0 s1 =
    let slice s =
      let length = String.length s in
      Bytes.Slice.make_or_eod (Bytes.unsafe_of_string s) ~first:0 ~length
    in
    equal_slices (slice s0) (slice s1)

  let equal_bytes b0 b1 =
    equal_slices (Bytes.Slice.of_bytes b0) (Bytes.Slice.of_bytes b1)

  external equal_bigbytes : bigbytes -> bigbytes -> bool =
    "ocaml_bytesrw_crypto_verify_equal_bigbytes"
end

module Hash = struct

  module Algorithm = struct
    type t =
    | Aes_mmo_zigbee
    | Md2 | Md4 | Md5
    | Ripemd_160
    | Sha3_224 | Sha3_256 | Sha3_384 | Sha3_512
    | Sha_1
    | Sha_224 | Sha_256 | Sha_384 | Sha_512 | Sha_512_224 | Sha_512_256
    | Shake256_512
    | Sm3

    let equal = ( = )
    let compare = Stdlib.compare
    let of_string s = match String.lowercase_ascii s with
    | "aes-mmo-zigbee" -> Ok Aes_mmo_zigbee
    | "md2" -> Ok Md2 | "md4" -> Ok Md4 | "md5" -> Ok Md5
    | "ripemd-160" -> Ok Ripemd_160
    | "sha-1" -> Ok Sha_1
    | "sha-224" -> Ok Sha_224 | "sha-256" -> Ok Sha_256
    | "sha-384" -> Ok Sha_384 | "sha-512" -> Ok Sha_512
    | "sha-512-224" -> Ok Sha_512_224 | "sha-512-256" -> Ok Sha_512_256
    | "sha3-224" -> Ok Sha3_224 | "sha3-256" -> Ok Sha3_256
    | "sha3-384" -> Ok Sha3_384 | "sha3-512" -> Ok Sha3_512
    | "shake256-512" -> Ok Shake256_512
    | "sm3" -> Ok Sm3
    | s -> Error (strf "Unknown hash algorithm: %S" s)

    let to_string = function
    | Aes_mmo_zigbee -> "aes-mmo-zigbee"
    | Md2 -> "md2" | Md4 -> "md4" | Md5 -> "md5"
    | Ripemd_160 -> "ripemd-160"
    | Sha3_224 -> "sha3-224" | Sha3_256 -> "sha3-256"
    | Sha3_384 -> "sha3-384" | Sha3_512 -> "sha3-512"
    | Sha_1 -> "sha-1"
    | Sha_224 -> "sha-224" | Sha_256 -> "sha-256"
    | Sha_384 -> "sha-384" | Sha_512 -> "sha-512"
    | Sha_512_224 -> "sha-512-224" | Sha_512_256 -> "sha-512-256"
    | Shake256_512 -> "shake256-512"
    | Sm3 -> "sm3"

    let pp ppf a = Format.pp_print_string ppf (to_string a)

    let to_psa_alg = function
    | Aes_mmo_zigbee -> Some Psa.Alg.aes_mmo_zigbee
    | Md2 -> Some Psa.Alg.md2 | Md4 -> Some Psa.Alg.md4
    | Md5 -> Some Psa.Alg.md5
    | Ripemd_160 -> Some Psa.Alg.ripemd160
    | Sha3_224 -> Some Psa.Alg.sha3_224 | Sha3_256 -> Some Psa.Alg.sha3_256
    | Sha3_384 -> Some Psa.Alg.sha3_384 | Sha3_512 -> Some Psa.Alg.sha3_512
    | Sha_1 -> Some Psa.Alg.sha_1
    | Sha_224 -> Some Psa.Alg.sha_224 | Sha_256 -> Some Psa.Alg.sha_256
    | Sha_384 -> Some Psa.Alg.sha_384 | Sha_512 -> Some Psa.Alg.sha_512
    | Sha_512_224 -> Some Psa.Alg.sha_512_224
    | Sha_512_256 -> Some Psa.Alg.sha_512_256
    | Shake256_512 -> Some Psa.Alg.shake256_512
    | Sm3 -> Some Psa.Alg.sm3

    module Map = Map.Make (struct type nonrec t = t let compare = compare end)

    let psa_algs =
      (* maps t to Psa.Alg.t. Unsupported algorithms are Psa.Alg.none *)
      ref Map.empty

    let psa_alg a = match Map.find_opt a !psa_algs with
    | Some psa_alg -> psa_alg
    | None ->
        let psa_alg = match to_psa_alg a with
        | None -> Psa.Alg.none
        | Some alg ->
            let op = Psa.Hash.Operation.init () in
            match Psa.Hash.setup op alg with
            | 0 -> alg
            | st ->
                if Psa.Status.equal st Psa.Error.not_supported
                then Psa.Alg.none
                else panic_status "psa_hash_setup" st
        in
        psa_algs := Map.add a psa_alg !psa_algs;
        psa_alg

    let is_supported a = not (Psa.Alg.equal Psa.Alg.none (psa_alg a))

    let get_psa_alg alg =
      let psa_alg = psa_alg alg in
      if Psa.Alg.equal Psa.Alg.none psa_alg
      then panicf "Hash algorithm %a unsupported" pp alg
      else psa_alg

    let length a = match to_psa_alg a with
    | None -> assert false (* For now *)
    | Some a -> Psa.Hash.length a
  end

  type t = string
  let length = String.length

  module State = struct
    type t = Psa.Hash.Operation.t * int (* the hash len *)
    let make alg =
      let palg = Algorithm.get_psa_alg alg in
      let hlen = Psa.Hash.length palg (* supported, should be non zero *) in
      let state = Psa.Hash.Operation.init () in
      match Psa.Hash.setup state palg with
      | 0 -> state, hlen | status -> panic_status "psa_hash_setup" status

    let update (state, _) slice = match Psa.Hash.update state slice with
    | 0 ->  () | status -> panic_status "psa_hash_update" status

    let copy (state, hlen) =
      let dst = Psa.Hash.Operation.init () in
      match Psa.Hash.clone ~src:state ~dst with
      | 0 -> dst, hlen | status -> panic_status "psa_hash_clone" status
  end

  let value (state, hlen) =
    let hash = Bytes.Slice.of_bytes (Bytes.create hlen) in
    match Psa.Hash.finish state ~hash with
    | Error st -> panic_status "psa_hash_finish" st
    | Ok len ->
        if hlen = len
        then Bytes.unsafe_to_string (Bytes.Slice.bytes hash)
        else panicf "Hash length is %d but expected %d" len hlen

  let verify_value (state, hlen) hash =
    let hash = Bytes.Slice.of_string_or_eod hash in
    match Psa.Hash.verify state ~hash with
    | 0 -> true
    | st when Psa.Status.equal st Psa.Error.invalid_signature -> false
    | st -> panic_status "psa_hash_verify" st

  let slice alg input =
    (* We use the single part function psa_hash_compute *)
    let palg = Algorithm.get_psa_alg alg in
    let hlen = Psa.Hash.length palg (* supported, should be non zero *) in
    let hash = Bytes.Slice.of_bytes (Bytes.create hlen) in
    match Psa.Hash.compute palg ~input ~hash with
    | Error st -> panic_status "psa_hash_compute" st
    | Ok len ->
        if hlen = len
        then Bytes.unsafe_to_string (Bytes.Slice.bytes hash)
        else panicf "%a hash length is %d but expected %d"
            Algorithm.pp alg len hlen

  let string alg s = slice alg (Bytes.Slice.of_string_or_eod s)
  let bytes alg b = slice alg (Bytes.Slice.of_bytes_or_eod b)
  let reader alg r =
    let rec loop state r = match Bytes.Reader.read r with
    | s when Bytes.Slice.is_eod s -> value state
    | s -> State.update state s; loop state r
    in
    loop (State.make alg) r

  let reads state r = Bytes.Reader.tap (State.update state) r
  let writes state w = Bytes.Writer.tap (State.update state) w

  let equal = Verify.equal_strings
  let to_binary_string = Fun.id
  let of_binary_string ?length s = match length with
  | None -> Ok s
  | Some length -> Bytesrw_hex.check_binary_string_length ~length s

  let to_hex = Bytesrw_hex.of_binary_string
  let of_hex = Bytesrw_hex.to_binary_string
  let pp ppf h = Format.pp_print_string ppf (to_hex h)

  module type T = sig
    val algorithm : Algorithm.t
    val id : string
    val length : int

    type t
    module State : sig
      type t
      val make : unit -> t
      val update : t -> Bytes.Slice.t -> unit
      val copy : t -> t
      (* Once we have PSA support for Hash.{suspend,resume} *)
        (*
       val to_binary_string : t -> string
       val of_binary_string : string -> t
      *)
    end

    val value : State.t -> t
    val verify_value : State.t -> t -> bool
    val string : string -> t
    val bytes : bytes -> t
    val slice : Bytes.Slice.t -> t
    val reader : Bytes.Reader.t -> t
    val reads : ?state:State.t -> Bytes.Reader.t -> Bytes.Reader.t * State.t
    val writes : ?state:State.t -> Bytes.Writer.t -> Bytes.Writer.t * State.t
    val equal : t -> t -> bool
    val to_binary_string : t -> string
    val of_binary_string : string -> (t, string) result
    val to_hex : t -> string
    val of_hex : string -> (t, string) result
    val pp : Format.formatter -> t -> unit
  end

  module type ALGORITHM = sig
    val algorithm : Algorithm.t
  end

  module Make (A : ALGORITHM) : T = struct
    let algorithm = A.algorithm
    let psa_alg = Algorithm.get_psa_alg algorithm
    let id = Algorithm.to_string algorithm
    let length = Algorithm.length algorithm
    type t = string

    module State = struct
      type t = Psa.Hash.Operation.t
      let make () =
        let state = Psa.Hash.Operation.init () in
        match Psa.Hash.setup state psa_alg with
        | 0 -> state | status -> panic_status "psa_hash_setup" status

      let update state slice = match Psa.Hash.update state slice with
      | 0 ->  () | status -> panic_status "psa_hash_update" status

      let copy state =
        let dst = Psa.Hash.Operation.init () in
        match Psa.Hash.clone ~src:state ~dst with
        | 0 -> dst | status -> panic_status "psa_hash_clone" status
    end

    let value state =
      let hash = Bytes.Slice.of_bytes (Bytes.create length) in
      match Psa.Hash.finish state ~hash with
      | Error st -> panic_status "psa_hash_finish" st
      | Ok len ->
          if length = len
          then Bytes.unsafe_to_string (Bytes.Slice.bytes hash)
          else panicf "%s hash length is %d but expected %d" id len length

    let verify_value state hash =
      let hash = Bytes.Slice.of_string_or_eod hash in
      match Psa.Hash.verify state ~hash with
      | 0 -> true
      | st when Psa.Status.equal st Psa.Error.invalid_signature -> false
      | st -> panic_status "psa_hash_verify" st

    let slice input =
      (* We use the single part function psa_hash_compute *)
      let hash = Bytes.Slice.of_bytes (Bytes.create length) in
      match Psa.Hash.compute psa_alg ~input ~hash with
      | Error st -> panic_status "psa_hash_compute" st
      | Ok len ->
          if length = len
          then Bytes.unsafe_to_string (Bytes.Slice.bytes hash)
          else panicf "%s hash length is %d but expected %d" id len length

    let string s = slice (Bytes.Slice.of_string_or_eod s)
    let bytes b = slice (Bytes.Slice.of_bytes_or_eod b)
    let reader r =
      let rec loop state r = match Bytes.Reader.read r with
      | s when Bytes.Slice.is_eod s -> value state
      | s -> State.update state s; loop state r
      in
      loop (State.make ()) r

    let reads ?(state = State.make ()) r =
      Bytes.Reader.tap (State.update state) r, state

    let writes ?(state = State.make ()) w =
      Bytes.Writer.tap (State.update state) w, state

    let equal = Verify.equal_strings
    let to_binary_string = Fun.id
    let of_binary_string s = Bytesrw_hex.check_binary_string_length ~length s
    let to_hex = Bytesrw_hex.of_binary_string
    let of_hex s = Bytesrw_hex.to_binary_string ~length s
    let pp ppf h = Format.pp_print_string ppf (to_hex h)
  end
end

module Sha_256 = Hash.Make (struct let algorithm = Hash.Algorithm.Sha_256 end)
module Sha_512 = Hash.Make (struct let algorithm = Hash.Algorithm.Sha_512 end)

(* Keys *)

module Key = struct

  type error = unit

  type t = { id : Psa.Key_id.t; }

  let invalid = { id = Psa.Key_id.null; }

  (* Comparisons and predicates *)

  let is_invalid k = Psa.Key_id.equal k.id Psa.Key_id.null

  let equal k0 k1 = Psa.Key_id.equal k0.id k1.id
  let compare k0 k1 = Psa.Key_id.compare k0.id k1.id
end

(* MAC *)

module Mac = struct
  module Algorithm = struct
    type t =
    | Hmac of Hash.Algorithm.t
    | Cbc_mac
    | Cmac
    | Truncated of t * int
    | Full_length of t
    | At_least_length of t * int
  end
end

(* Random *)

module Random = struct

  (* Primitive *)

  let set_random slice = match Psa.generate_random slice with
  | 0 -> () | rc -> raise (Panic (err_rand rc))

  let set_random_or_error_stream slice = match Psa.generate_random slice with
  | 0 -> slice | rc -> stream_error (err_rand rc)

  (* Cryptographically secure pseurorandom bytes *)

  let check_length l =
    if l >= 0 then l else
    invalid_arg (strf "Invalid random length: %d is negative" l)

  let reads ?pos ?slice_length ?length () =
    let default = Bytes.Slice.default_length in
    let slice_length = Option.value ~default slice_length in
    match length with
    | None -> (* infinite stream *)
        let slice = Bytes.Slice.of_bytes (Bytes.create slice_length) in
        let read () = set_random slice; slice in
        Bytes.Reader.make ?pos ~slice_length read
    | Some 0 -> Bytes.Reader.empty ?pos ~slice_length ()
    | Some len ->
        let slice_length = Int.min (check_length len) slice_length in
        let slice = Bytes.Slice.of_bytes_or_eod (Bytes.create slice_length) in
        let rem = ref len in
        let read () = match !rem with
        | 0 -> Bytes.Slice.eod
        | n ->
            let slice =
              if n >= Bytes.Slice.length slice then slice else
              Bytes.Slice.make (Bytes.Slice.bytes slice) ~first:0 ~length:n
            in
            rem := n - Bytes.Slice.length slice;
            set_random slice; slice
        in
        Bytes.Reader.make ?pos ~slice_length read

  let bytes n =
    let b = Bytes.create n in
    set_random (Bytes.Slice.of_bytes_or_eod b);
    b

  let string n = Bytes.unsafe_to_string (bytes n)
end
