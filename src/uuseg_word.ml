(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   WB1.                 sot ÷
   WB2.                     ÷ eot
   WB3.                  CR × LF
   WB3a.         (NL|CR|LF) ÷
   WB3b.                    ÷ (NL|CR|LF)
   WB4.      X (Extend|FO)* → X
   WB5.             (LE|HL) × (LE|HL)
   WB6.             (LE|HL) × (ML|MB|SQ) (LE|HL)
   WB7.  (LE|HL) (ML|MB|SQ) × (LE|HL)
   WB7a.                 HL × SQ
   WB7b.                 HL × DQ HL
   WB7c.              HL DQ × HL
   WB8.                  NU × NU
   WB9.             (LE|HL) × NU
   WB10.                 NU × (LE|HL)
   WB11.      NU (MN|MB|SQ) × NU
   WB12.                 NU × (MN|MB|SQ) NU
   WB13.                 KA × KA
   WB13a.  (LE|HL|NU|KA|EX) × EX
   WB13b.                EX × (LE|HL|NU|KA)
   WB13c.                RI × RI
   WB14.                Any ÷ Any

   [1]: http://www.unicode.org/reports/tr29/#Word_boundaries
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt

   Given the structure of the rules we keep a window of four word
   break property value slots, two on the left, two on the right of a
   boundary and pattern match these slots to find the rule that
   applies. Because of WB4 these slots may actually correspond to more
   than one character and we need to bufferize the data for the two
   slots on the right.
                            ---??--->
                     +----+----++----+----+
                  ...| l1 | l0 || r0 | r1 |
                     +----+----++----+----+
   already returned to client /  \ buffered in segmenter *)

type word =
| CR | DQ | EX | Extend | FO | HL | KA | LE | LF | MB | ML | MN
| NL | NU | RI | SQ | XX | Invalid | Sot | Eot

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.word. *)

let byte_to_word =
  [| CR; DQ; EX; Extend; FO; HL; KA; LE; LF; MB; ML; MN; NL; NU; RI; SQ; XX; |]

let word u = byte_to_word.(Uucp.Break.Low.word u)

type state =
  | Fill  (* fill slots on the right of boundary. *)
  | Flush (* flush slot r0 to get to next boundary. *)
  | End   (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    window : word array;                                    (* break window. *)
    mutable l0 : int;                            (* index in [window] of l0. *)
    rbufs : Uuseg_buf.t array;(* buffers for slots on the right of boundary. *)
    mutable r0_bufs : int;      (* index of buffer for slot [r0] in [rbufs]. *)
    mutable rfill : int; }                   (* index of right slot to fill. *)

let create () =
  { state = Fill;
    window = [|Invalid; Sot; Invalid; Invalid|];
    l0 = 1;
    rbufs = [| Uuseg_buf.create 13; Uuseg_buf.create 13; |];
    r0_bufs = 0;
    rfill = -1; }

let copy s =
  let copy_rbuf i = Uuseg_buf.copy s.rbufs.(i) in
  { s with window = Array.copy s.window;
           rbufs = Array.init (Array.length s.rbufs) copy_rbuf; }

let new_slot s = s.rfill <- s.rfill + 1
let slot_add s add =
  Uuseg_buf.add s.rbufs.((s.r0_bufs + s.rfill) mod Array.length s.rbufs) add

let slot_set s word =
  s.window.((s.l0 + s.rfill + 1) mod Array.length s.window) <- word

let slot_word s = (* N.B. this will lookup sot in l0 on the first add *)
  s.window.((s.l0 + s.rfill + 1) mod Array.length s.window)

let l0_word s = s.window.(s.l0)
let r0_empty s = Uuseg_buf.empty s.rbufs.(s.r0_bufs)
let r0_flush s = Uuseg_buf.flush s.rbufs.(s.r0_bufs)
let r0_word s = s.window.((s.l0 + 1) mod Array.length s.window)

let window_full s = s.rfill + 1 = Array.length s.rbufs
let window_move s =
  let wlen = Array.length s.window in
  if s.state = End then (s.window.((s.l0 + wlen / 2) mod wlen) <- Invalid);
  s.l0 <- (s.l0 + 1) mod wlen;
  s.r0_bufs <- (s.r0_bufs + 1) mod Array.length s.rbufs;
  s.rfill <- s.rfill - 1

let decidable s = window_full s || s.state = End
let decide s =
  let no_boundary s = Uuseg_buf.flush s.rbufs.(s.r0_bufs) in
  let wlen = Array.length s.window in
  let l0 = s.l0 in
  let r0 = (l0 + 1) mod wlen in
  let r1 = (l0 + 2) mod wlen in
  let l1 = (l0 + 3) mod wlen in
  let w = s.window in
  match w.(l1), w.(l0) (**),(**) w.(r0), w.(r1) with
  | (* WB1 *)   _, Sot, _, _ -> `Boundary
  (* WB2 is handled in [add]. *)
  | (* WB3 *)   _, CR, LF, _ -> no_boundary s
  | (* WB3a *)  _, (NL|CR|LF), _, _ -> `Boundary
  | (* WB3b *)  _, _, (NL|CR|LF), _ -> `Boundary
  (* WB4 is handled indirectly during Fill *)
  | (* WB5 *)   _, (LE|HL), (LE|HL), _ -> no_boundary s
  | (* WB6 *)   _, (LE|HL), (ML|MB|SQ), (LE|HL) -> no_boundary s
  | (* WB7 *)   (LE|HL), (ML|MB|SQ), (LE|HL), _ -> no_boundary s
  | (* WB7a *)  _, HL, SQ, _ -> no_boundary s
  | (* WB7b *)  _, HL, DQ, HL -> no_boundary s
  | (* WB7c *)  HL, DQ, HL, _ -> no_boundary s
  | (* WB8 *)   _, NU, NU, _ -> no_boundary s
  | (* WB9 *)   _, (LE|HL), NU, _ -> no_boundary s
  | (* WB10 *)  _, NU, (LE|HL), _ -> no_boundary s
  | (* WB11 *)  NU, (MN|MB|SQ), NU, _ -> no_boundary s
  | (* WB12 *)  _, NU, (MN|MB|SQ), NU -> no_boundary s
  | (* WB13 *)  _, KA, KA, _ -> no_boundary s
  | (* WB13a *) _, (LE|HL|NU|KA|EX), EX, _ -> no_boundary s
  | (* WB13b *) _, EX, (LE|HL|NU|KA), _ -> no_boundary s
  | (* WB13c *) _, RI, RI, _ -> no_boundary s
  | (* WB14  *) _, _, _, _ -> `Boundary

let add s = function
| `Uchar u as add ->
    begin match s.state with
    | Fill ->
        begin match word u with
        | Extend | FO as word ->
            (* WB4 *)
            begin match slot_word s with
            | NL | CR | LF | Sot ->
                new_slot s; slot_set s word; slot_add s add;
                if decidable s then (s.state <- Flush; decide s) else `Await
            | _ -> slot_add s add; `Await
            end
        | word ->
            new_slot s; slot_set s word; slot_add s add;
            if decidable s then (s.state <- Flush; decide s) else `Await
        end
    | Flush -> Uuseg_base.err_exp_await add
    | End -> Uuseg_base.err_ended add
    end
| `Await ->
    begin match s.state with
    | Flush ->
        if not (r0_empty s) then r0_flush s else
        (s.state <- Fill; window_move s; `Await)
    | End ->
        if not (r0_empty s) then r0_flush s else
        begin match r0_word s with
        | Invalid -> `End
        | Eot ->
            (* WB2 *)
            if l0_word s = Sot then `End else (window_move s; `Boundary)
        | _ -> window_move s; decide s
        end
    | Fill -> `Await
    end
| `End ->
    begin match s.state with
    | Fill -> s.state <- End; new_slot s; slot_set s Eot; decide s
    | Flush -> Uuseg_base.err_exp_await `End
    | End -> Uuseg_base.err_ended `End
    end

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
