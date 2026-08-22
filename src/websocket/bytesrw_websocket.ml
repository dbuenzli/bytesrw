(*---------------------------------------------------------------------------
   Copyright (c) 2026 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Section numbers in the commments refer to RFC 6455. *)

open Bytesrw

let strf = Format.asprintf
let set_if_none a v = ignore (Atomic.compare_and_set a None (Some v))
let stack_errors r0 r1 = match r0, r1 with
| Ok (), Ok () -> Ok ()
| Ok _, (Error _ as e) | (Error _ as e), Ok _ -> e
| Error e0, Error e1 -> Error (String.concat "\n" [e0; e1])

let xor_payload ~mask b ~length =
  for i = 0 to length - 1 do
    let u = Bytes.get_uint8 b i in
    let m = Bytes.get_uint8 mask (i mod 4) in
    Bytes.set_uint8 b i (u lxor m)
  done

(* Errors and status codes *)

type error =
| Shutdown
| Recv_message_byte_size_exceeded of int

type Bytes.Stream.error += Error of error

let error =
  let case e = Error e in
  let message = function
  | Error Shutdown -> "WebSocket is closing or closed"
  | Error Recv_message_byte_size_exceeded max ->
      strf "Received message size exceeds %d bytes" max
  | _ -> assert false
  in
  Bytes.Stream.make_format_error ~format:"websocket" ~case ~message

let shutdown_reader r () = Bytes.Reader.error error r Shutdown
let shutdown_writer w () = Bytes.Writer.error error w Shutdown
let reader_recv_message_byte_size_exceeded r ~max =
  Bytes.Reader.error error r (Recv_message_byte_size_exceeded max)

type status_code = int

let utf_8_error = 1007
let status_code_meaning = function
| 1000 -> "Normal closure"
| 1001 -> "Going away"
| 1002 -> "Protocol error"
| 1003 -> "Unsupported data"
| 1005 -> "No status received"
| 1006 -> "Abnormal closure"
| 1007 -> "Invalid frame payload data"
| 1008 -> "Policy violation"
| 1009 -> "Message too big"
| 1010 -> "Mandatory ext."
| 1011 -> "Internal server error"
| 1015 -> "TLS handshake"
| c -> "Unknown"

let pp_status_code ppf st =
  Format.fprintf ppf "%d (%s)" st (status_code_meaning st)

(* Sides *)

type side = Client | Server
let pp_side ppf = function
| Client -> Format.pp_print_string ppf "client"
| Server -> Format.pp_print_string ppf "server"

(* Connection *)

type synchronized = { synchronized : 'a. (unit -> 'a) -> 'a }

let max_ctrl_frame_payload_byte_size = 125 (* §5.5 *)
let default_max_recv_message_byte_size = 128 * 1024

type state = Open | Closing | Closed
type t =
  { side : side;
    close : (unit -> (unit, string) result);
    set_crypto_random : Bytes.Slice.t -> unit;
    state : state Atomic.t;
    our_close_status : (status_code * string) option Atomic.t;
    peer_close_status : status_code option Atomic.t;
    send_synchronized : 'a. (unit -> 'a) -> 'a;
    send : Bytes.Writer.t;
    send_buf : Bytes.t;
    mask_buf : Bytes.Slice.t;
    recv_synchronized : 'a. (unit -> 'a) -> 'a;
    recv : Bytes.Reader.t;
    mutable recv_rem : Bytes.Slice.t;
    recv_buf : Bytes.t;
    recv_mask : Bytes.t;
    mutable last_recv_message_is_text : bool;
    max_recv_message_byte_size : int; }

let not_synchronized () = { synchronized = fun f -> f () }

let make
    ?(make_synchronized = not_synchronized)
    ?(max_recv_message_byte_size = max_int) ?set_crypto_random
    ?(close = Fun.const (Ok ())) ~send ~recv side
  =
  let set_crypto_random =
    Option.value ~default:Bytesrw_sysrandom.set_random set_crypto_random
  in
  let state = Atomic.make Open in
  let our_close_status = Atomic.make None in
  let peer_close_status = Atomic.make None in
  let { synchronized = send_synchronized } = make_synchronized () in
  let send_buf_len = match side with
  | Server -> 10 (* Only used for sending the frame header *)
  | Client ->
      (* The buffer is also used for masking, we make sure to have at least
         10 bytes and a multiple of 4 *)
      Int.max ((Bytes.Writer.slice_length send / 4) * 4) 12
  in
  let send_buf = Bytes.create send_buf_len in
  let mask_buf = match side with
  | Server -> Bytes.Slice.eod (* Won't be touched *)
  | Client -> Bytes.Slice.of_bytes (Bytes.create 4)
  in
  let { synchronized = recv_synchronized } = make_synchronized () in
  let recv_buf = Bytes.create 8 in
  let recv_rem = Bytes.Slice.eod in
  let recv_mask = match side with
  | Server -> Bytes.create 4
  | Client -> Bytes.empty (* Won't be touched *)
  in
  let last_recv_message_is_text = false in
  { side; close; set_crypto_random;
    state; our_close_status; peer_close_status;
    send_synchronized; send; send_buf; mask_buf;
    recv_synchronized; recv; recv_rem; recv_buf; recv_mask;
    last_recv_message_is_text; max_recv_message_byte_size;  }

let last_recv_message_is_text c = c.last_recv_message_is_text
let max_recv_message_byte_size c = c.max_recv_message_byte_size
let side c = c.side
let is_shutdown c = not (Atomic.get c.state = Open)
let our_close_status c = Atomic.get c.our_close_status
let peer_close_status c = Atomic.get c.peer_close_status

(* Sending *)

let write_frame_header c ~fin ~op ~payload_length =
  (* Must be called in c.send_synchronized *)
  let fin_bit = if fin then 0x80 else 0x00 in
  let has_mask = Bytes.Slice.length c.mask_buf > 0 in
  let mask_bit = if has_mask then 0x80 else 0x00 in
  Bytes.set_uint8 c.send_buf 0 (fin_bit lor op);
  let length = match payload_length with
  | l when l <= 125 ->
      Bytes.set_uint8 c.send_buf 1 (mask_bit lor l);
      2
  | l when l <= 65525 ->
      Bytes.set_uint8 c.send_buf 1 (mask_bit lor 126);
      Bytes.set_uint16_be c.send_buf 2 payload_length;
      4
  | l ->
      Bytes.set_uint8 c.send_buf 1 (mask_bit lor 127);
      Bytes.set_int64_be c.send_buf 2 (Int64.of_int payload_length);
      10
  in
  let length =
    if not has_mask then length else
    let send_mask = Bytes.Slice.bytes c.mask_buf in
    (Bytes.blit send_mask 0 c.send_buf length 4; length + 4)
  in
  Bytes.Writer.write c.send (Bytes.Slice.make c.send_buf ~first:0 ~length)

let write_masked_payload c payload =
  (* Must be called in c.send_synchronized and Bytes.Slice.length payload > 0 *)
  let rec loop c b first length =
    let take = Int.min length (Bytes.length c.send_buf) in
    let slice = Bytes.Slice.make c.send_buf ~first:0 ~length:take in
    Bytes.blit b first c.send_buf 0 take;
    (* This work because [take mod 4 = 0] or it's the final iteration. *)
    xor_payload ~mask:(Bytes.Slice.bytes c.mask_buf) c.send_buf ~length:take;
    Bytes.Writer.write c.send slice;
    if take < length then loop c b (first + take) (length - take) else ()
  in
  let first = Bytes.Slice.first payload in
  let length = Bytes.Slice.length payload in
  loop c (Bytes.Slice.bytes payload) first length

let write_frame c ~fin ~op payload =
  (* Must be called in c.send_synchronized *)
  let payload_length = Bytes.Slice.length payload in
  match c.side with
  | Server ->
      write_frame_header c ~fin ~op ~payload_length;
      if payload_length > 0 then Bytes.Writer.write c.send payload
  | Client ->
      c.set_crypto_random c.mask_buf;
      write_frame_header c ~fin ~op ~payload_length;
      if payload_length > 0 then write_masked_payload c payload

let synchronized_write_frame c ~fin ~op payload =
  c.send_synchronized @@ fun () ->
  if is_shutdown c
  then shutdown_writer c.send ()
  else write_frame c ~fin ~op payload

let send ?(text = false) c s =
  let payload = Bytes.Slice.of_bytes_or_eod (Bytes.unsafe_of_string s) in
  let op = if text then 0x01 else 0x02 in
  synchronized_write_frame c ~fin:true ~op payload

let send_reader ?(text = false) c r =
  let rec loop ~op c r =
    let slice = Bytes.Reader.read r in
    if Bytes.Slice.is_eod slice
    then synchronized_write_frame c ~fin:true ~op slice
    else (synchronized_write_frame c ~fin:false ~op slice; loop ~op:0x00 c r)
  in
  loop ~op:(if text then 0x01 else 0x02) c r

let send_writes ?(text = false) c f = failwith "TODO"

let send_ping c = synchronized_write_frame c ~fin:true ~op:0x09 Bytes.Slice.eod
let send_pong c payload = synchronized_write_frame c ~fin:true ~op:0x0A payload

(* Receiving *)

let err_recv c st msg =
  ignore (Atomic.compare_and_set c.state Open Closing);
  set_if_none c.our_close_status (st, msg);
  shutdown_reader c.recv ()

let err_unexpected_eod c = err_recv c 1002 "peer closed abnormally"
let err_unmasked c = err_recv c 1002 "client peer sent unmasked frame"
let err_masked c = err_recv c 1002 "server peer sent masked frame"
let err_unexpected_fragment c = err_recv c 1002 "peer send unexpected fragment"
let err_empty_fragment c =
  err_recv c 1002 "peer send a fragment without payload"

let err_truncated c = err_recv c 1002 "peer sent truncated message"
let err_truncated_close c = err_recv c 1002 "peer sent truncated close frame"
let err_overized_ctrl_frame c =
  err_recv c 1002
    (strf "peer sent an overized control frame (max is %d)"
       max_ctrl_frame_payload_byte_size)

let err_oversized c =
  ignore (Atomic.compare_and_set c.state Open Closing);
  let max = c.max_recv_message_byte_size in
  let msg = strf "peer sent an oversized message (our limit is %d bytes)" max in
  set_if_none c.our_close_status (1009, msg);
  reader_recv_message_byte_size_exceeded c.recv ~max

let err_invalid_opcode c op =
  err_recv c 1002 (strf "peer sent invalid opcode (x%x)" (Char.code op))

let is_data_frame = function '\x00' .. '\x02' -> true | _ -> false
let is_fragment_frame c = Char.equal c '\x00'
let is_text_frame c = Char.equal c '\x01'
let is_ctrl_frame c = not (is_data_frame c)

let update_last_recv_message_is_text c = function
| '\x01' -> c.last_recv_message_is_text <- true
| '\x02' -> c.last_recv_message_is_text <- false
| _ -> ()

let read_exactly c ~into:buf ~need =
  (* Must be called in c.recv_synchronized *)
  let rec loop c buf i need =
    if need = 0 then () else begin
      if Bytes.Slice.is_eod c.recv_rem
      then c.recv_rem <- Bytes.Reader.read c.recv;
      if Bytes.Slice.is_eod c.recv_rem
      then err_unexpected_eod c;
      let first = Bytes.Slice.first c.recv_rem in
      let length = Bytes.Slice.length c.recv_rem in
      let bytes = Bytes.Slice.bytes c.recv_rem in
      let take = Int.min need length in
      Bytes.blit bytes first buf i take;
      let () =
        c.recv_rem <- match Bytes.Slice.drop_first take c.recv_rem with
        | None -> Bytes.Slice.eod
        | Some rem -> rem
      in
      if take < need then loop c buf (i + take) (need - take) else ()
    end
  in
  loop c buf 0 need

let close_recv c =
  if Bytes.Slice.is_eod c.recv_rem then () else
  Bytes.Reader.push_back c.recv c.recv_rem

let read_frame_header c =
  (* Must be called in c.recv_synchronized *)
  let () = read_exactly c ~into:c.recv_buf ~need:2 in
  let b0 = Bytes.get_uint8 c.recv_buf 0 in
  let b1 = Bytes.get_uint8 c.recv_buf 1 in
  let fin = (b0 land 0x80) <> 0 in
  let op = Char.chr (b0 land 0x0F) in
  let is_masked = (b1 land 0x80) <> 0 in
  begin match c.side with
  | Server -> if not is_masked then err_unmasked c
  | Client -> if is_masked then err_masked c
  end;
  let payload_len = match b1 land 0x7f with
  | 126 ->
      let () = read_exactly c ~into:c.recv_buf ~need:2 in
      Bytes.get_uint16_be c.recv_buf 0
  | 127 ->
      let () = read_exactly c ~into:c.recv_buf ~need:8 in
      let payload_len = Bytes.get_int64_be c.recv_buf 0 in
      begin match Int64.unsigned_to_int payload_len with
      | None -> err_oversized c
      | Some payload_len -> payload_len
      end
  | payload_len -> payload_len
  in
  (if is_ctrl_frame op && payload_len > max_ctrl_frame_payload_byte_size
   then err_oversized c);
  ((* N.B. this does not abide to the RFC, see .mli. Note that if we stop doing
      this, adjustments are needed in [recv] to bytes reader/writer in order
      not to generate Bytes.Slice.eod *)
    if is_fragment_frame op && not fin && payload_len = 0
    then err_empty_fragment c);
  (if is_masked then read_exactly c ~into:c.recv_mask ~need:4);
  update_last_recv_message_is_text c op;
  fin, op, payload_len

let read_frame_payload c ~payload_len =
  (* Must be called in c.recv_synchronized *)
  let b = Bytes.create payload_len in
  let () = read_exactly c ~into:b ~need:payload_len in
  let is_masked = Bytes.length c.recv_mask > 0 in
  (if is_masked then xor_payload ~mask:c.recv_mask b ~length:payload_len);
  Bytes.unsafe_to_string b

let read_frame ~quota c =
  (* Must be called in c.recv_synchronized *)
  let fin, op, payload_len = read_frame_header c in
  if is_data_frame op && quota - payload_len < 0 then err_oversized c else
  let p = read_frame_payload c ~payload_len in
  fin, op, p

let synchronized_read_frame ~quota c =
  c.recv_synchronized @@ fun () ->
  if is_shutdown c
  then shutdown_reader c.recv ()
  else read_frame ~quota c

let handle_close_frame c p = (* returns [false] on errors *)
  if String.length p = 0
  then (set_if_none c.peer_close_status 1005 (* §7.1.5 *); true) else
  if String.length p < 2
  then false (* there MUST be a status *)
  else (set_if_none c.peer_close_status (String.get_uint16_be p 0); true)

let rec synchronized_read_data_frame ~quota ~expect_frag c =
  let fin, op, p = synchronized_read_frame ~quota c in
  match op with
  | '\x00' (* continuation *) ->
      if not expect_frag then err_unexpected_fragment c else (fin, p)
  | '\x01' (* text *) | '\x02' (* binary *) ->
      if expect_frag then err_truncated c else (fin, p)
  | '\x08' (* close *) ->
      if not (handle_close_frame c p) then err_truncated_close c else
      if expect_frag then err_truncated c else shutdown_reader c.recv ()
  | '\x09' (* ping *) ->
      (* TODO after read_frame refactoring to local buffer *)
      let p = Bytes.Slice.of_string_or_eod p in
      send_pong c p;
      synchronized_read_data_frame ~quota ~expect_frag c
  | '\x0A' (* pong *) -> (* we don't care *)
      synchronized_read_data_frame ~quota ~expect_frag c
  | '\x03' .. '\x07' | '\x0B' .. '\x0F' -> err_invalid_opcode c op
  | _ -> assert false

let recv c =
  let rec loop ~quota ~expect_frag acc c =
    let fin, p = synchronized_read_data_frame ~expect_frag ~quota c in
    if fin
    then String.concat "" (List.rev (p :: acc))
    else loop ~quota:(quota - String.length p) ~expect_frag:true (p :: acc) c
  in
  loop ~quota:c.max_recv_message_byte_size ~expect_frag:false [] c

type reader_state = First | Next | End

let recv_reader c =
  let quota = ref c.max_recv_message_byte_size in
  let state = ref First in
  let rec next () =
    if !state = End then Bytes.Slice.eod else
    let q = !quota and expect_frag = !state = Next in
    let fin, p = synchronized_read_data_frame ~expect_frag ~quota:q c in
    state := if fin then End else Next;
    Bytes.Slice.of_bytes (Bytes.unsafe_of_string p)
  in
  let slice_length = Bytes.Reader.slice_length c.recv in
  Bytes.Reader.make ~slice_length next

let write_recv w c =
  let rec loop ~quota ~expect_frag c w =
    let fin, p = synchronized_read_data_frame ~quota ~expect_frag c in
    let p = Bytes.Slice.of_bytes_or_eod (Bytes.unsafe_of_string p) in
    if not (Bytes.Slice.is_eod p) then Bytes.Writer.write w p;
    if fin then () else
    loop ~quota:(quota - Bytes.Slice.length p) ~expect_frag:true c w
  in
  loop ~quota:c.max_recv_message_byte_size ~expect_frag:false c w

(* Closing *)

let err_closing_handshake msg = Result.Error ("closing handshake: " ^ msg)
let err_close msg = Result.Error ("WebSocket close: " ^ msg)

let close_status_code c = Option.value ~default:1006 (peer_close_status c)
let closed_normally c = match our_close_status c, peer_close_status c with
| Some (1000, _), Some 1000 -> true
| _ -> false

let close_message c =
  let pp_none ppf () = Format.pp_print_string ppf "<none>" in
  let pp_peer_status ppf = function
  | None -> pp_none ppf ()
  | Some st -> pp_status_code ppf st
  in
  let pp_our_status ppf = function
  | None -> pp_none ppf ()
  | Some (st, "") -> pp_status_code ppf st
  | Some (st, msg) ->
      Format.fprintf ppf "%d (%s: %s)" st (status_code_meaning st) msg
  in
  match our_close_status c, peer_close_status c with
  | None, None -> "Not closed yet"
  | Some (1000, _), Some 1000 -> "Normal closure"
  | ours, peer ->
      strf "@[Abnormal closure – @[Ours: %a@ Peer: %a@]@]"
        pp_our_status ours pp_peer_status peer

let set_our_close_status c ~suggested_status:sugg ~default = match sugg with
| Some st -> set_if_none c.our_close_status (st, "suggested on close"); st
| None -> set_if_none c.our_close_status default; (fst default)

let synchronized_write_close_noerr c ~status =
  c.send_synchronized @@ fun () ->
  try
    let payload = Bytes.create 2 in
    Bytes.set_uint16_be payload 0 status;
    write_frame c ~fin:true ~op:0x08 (Bytes.Slice.of_bytes payload);
    Bytes.Writer.write_eod c.send;
    Ok ()
  with
  | Bytes.Stream.Error e ->
      (try Bytes.Writer.write_eod c.send
      with Bytes.Stream.Error _ -> ());
      err_closing_handshake (Bytes.Stream.error_message e)

let synchronized_read_close_noerr c =
  c.recv_synchronized @@ fun () ->
  try match peer_close_status c with
  | Some _ -> Ok ()
  | None ->
      let rec loop ~start_pos ~frame_count c =
        (* Only try to get the close frame in 4 frames or 1KiB *)
        if (Bytes.Reader.pos c.recv - start_pos + 1 > 1024 || frame_count >= 4)
        then err_closing_handshake "Gave up receiving close frame"
        else match read_frame ~quota:1024 c with
        | exception Bytes.Stream.Error (Error Shutdown, _) ->
            err_closing_handshake "End of data before receiving close frame"
        | _fin, op, p ->
            if op = '\x08'
            then (ignore (handle_close_frame c p); Ok ())
            else (loop ~start_pos ~frame_count:(frame_count + 1) c)
      in
      loop ~start_pos:(Bytes.Reader.pos c.recv) ~frame_count:0 c
  with
  | Bytes.Stream.Error e -> Bytes.Stream.error_to_result e

let close ?suggested_status c =
  if Atomic.exchange c.state Closed = Closed then Ok () else
  begin
    let handshake = match peer_close_status c with
    | Some status ->
        let ours = match our_close_status c with
        | Some (status, _) -> status
        | None ->
            let default = status, "received by peer" in
            set_our_close_status c ~suggested_status ~default
        in
        synchronized_write_close_noerr c ~status:ours
    | None ->
        let ours = match our_close_status c with
        | Some (status, _) -> status
        | None -> set_our_close_status c ~suggested_status ~default:(1000, "")
        in
        let send = synchronized_write_close_noerr c ~status:ours in
        let recv = synchronized_read_close_noerr c in
        match send, recv with
        | Ok (), Ok () -> Ok ()
        | Ok _, (Error _ as e) | (Error _ as e), Ok _ -> e
        | Error e0, Error e1 -> Error (String.concat "\n" [e0; e1])
    in
    let close = if closed_normally c then Ok () else Error (close_message c) in
    let () = close_recv c in
    let user_close = c.close () in
    Result.map_error (fun e -> "WebSocket close: " ^ e) @@
    stack_errors handshake @@
    stack_errors close @@
    user_close
  end
