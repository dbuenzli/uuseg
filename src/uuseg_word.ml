(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   WB1.                 sot ÷ Any
   WB2.                 Any ÷ eot
   WB3.                  CR × LF
   WB3a.         (NL|CR|LF) ÷
   WB3b.                    ÷ (NL|CR|LF)
   WB3c. (v10.0.0)      ZWJ × (Glue_After_Zwj|EBG)
   WB3d.          WSegSpace × WSegSpace
   WB4.  X (Extend|FO|ZWJ)* → X
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
   WB14. (v10.0.0) (EB|EBG) × EM
   WB15     sot (RI RI)* RI × RI
   WB15   [^RI] (RI RI)* RI × RI
   WB13c.                RI × RI
   WB999.               Any ÷ Any

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
| CR | DQ | EX | EB | EBG | EM | Extend | FO | GAZ | HL | KA | LE | LF
| MB | ML | MN | NL | NU | RI | SQ | WSegSpace | XX | ZWJ | Invalid | Sot | Eot

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.word. *)

let byte_to_word =
  [| CR; DQ; EX; EB; EBG; EM; Extend; FO; GAZ; HL; KA; LE; LF;
     MB; ML; MN; NL; NU; RI; SQ; WSegSpace; XX; ZWJ |]

let word u = byte_to_word.(Uucp.Break.Low.word u)

type state =
  | Fill  (* fill slots on the right of boundary. *)
  | Flush (* flush slot r0 to get to next boundary. *)
  | End   (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    window : word array;                                    (* break window. *)
    window_is_zwj : bool array;                     (* for WB3c despite WB4. *)
    mutable l0 : int;                            (* index in [window] of l0. *)
    rbufs : Uuseg_buf.t array;(* buffers for slots on the right of boundary. *)
    mutable r0_bufs : int;      (* index of buffer for slot [r0] in [rbufs]. *)
    mutable rfill : int;                     (* index of right slot to fill. *)
    mutable odd_ri : bool;      (* odd number of RI on the left of boundary. *)
  }

let create () =
  { state = Fill;
    window = [|Invalid; Sot; Invalid; Invalid|];
    window_is_zwj = [| false; false; false; false|];
    l0 = 1;
    rbufs = [| Uuseg_buf.create 13; Uuseg_buf.create 13; |];
    r0_bufs = 0;
    rfill = -1;
    odd_ri = false; }

let copy s =
  let copy_rbuf i = Uuseg_buf.copy s.rbufs.(i) in
  { s with window = Array.copy s.window;
           rbufs = Array.init (Array.length s.rbufs) copy_rbuf; }

let new_slot s = s.rfill <- s.rfill + 1
let slot_add s add =
  Uuseg_buf.add s.rbufs.((s.r0_bufs + s.rfill) mod Array.length s.rbufs) add

let slot_set s word =
  let i = (s.l0 + s.rfill + 1) mod Array.length s.window in
  s.window.(i) <- word; s.window_is_zwj.(i) <- (word = ZWJ)

let slot_set_is_zwj s is_zwj =
  s.window_is_zwj.((s.l0 + s.rfill + 1) mod Array.length s.window) <- is_zwj

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
  if s.window.(s.l0) = RI then s.odd_ri <- not s.odd_ri else s.odd_ri <- false;
  s.r0_bufs <- (s.r0_bufs + 1) mod Array.length s.rbufs;
  s.rfill <- s.rfill - 1

(* The following is totally incomprehensible we need to
   move to a better model. *)

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
  | (* ε *)     _, Sot, Eot, _ -> `End
  | (* WB1-2 *) _, (Sot|Eot), _, _ -> `Boundary
  | (* WB3 *)   _, CR, LF, _ -> no_boundary s
  | (* WB3a *)  _, (NL|CR|LF), _, _ -> `Boundary
  | (* WB3b *)  _, _, (NL|CR|LF), _ -> `Boundary
  | (* WB3c *)  _, _,(GAZ|EBG), _ when s.window_is_zwj.(l0) -> no_boundary s
  | (* WB3d *)  _, WSegSpace,WSegSpace, _ -> no_boundary s
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
  | (* WB14 *)  _, (EB|EBG),EM, _ -> no_boundary s
  | (* WB15-16 *) _, RI, RI, _ when s.odd_ri -> no_boundary s
  | (* WB999 *) _, _, _, _ -> `Boundary

let add s = function
| `Uchar u as add ->
    begin match s.state with
    | Fill ->
        begin match word u with
        | Extend | FO | ZWJ as word ->
            (* WB4 *)
            begin match slot_word s with
            | NL | CR | LF | Sot ->
                new_slot s; slot_set s word; slot_add s add;
                if decidable s then (s.state <- Flush; decide s) else `Await
            | _ ->
                slot_set_is_zwj s (word = ZWJ);
                slot_add s add; `Await
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
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
