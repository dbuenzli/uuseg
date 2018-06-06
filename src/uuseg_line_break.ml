(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1]
   LB1 (per suggestion)
                           (AI|SG|XX) → AL
                      SA when (Mn|Mc) → CM
                                   SA → AL
                                   CJ → NS
   LB2                            sot ×
   LB3                                ! eot
   LB4                             BK !
   LB5                             CR × LF
                           (CR|LF|NL) !
   LB6                                × (BK|CR|LF|NL)
   LB7                                × (SP|ZW)
   LB8                         ZW SP* ÷
   LB8a                           ZWJ ×
   LB9  ¬(BK|CR|LF|NL|SP|ZW as X) (CM|ZWJ) * → X
   LB10                      (CM|ZWJ) → AL
   LB11                               × WJ
                                   WJ ×
   LB12                            GL ×
   LB12a                  ¬(SP|BA|HY) × GL
   LB13                               × (CL|CP|EX|IS|SY)
   LB14                        OP SP* ×
   LB15                        QU SP* × OP
   LB16                   (CL|CP) SP* × NS
   LB17                        B2 SP* × B2
   LB18                            SP ÷
   LB19                               × QU
                                   QU ×
   LB20                               ÷ CB
                                   CB ÷
   LB21                               × (BA|HY|NS)
                                   BB ×
   LB21a                   HL (HY|BA) ×
   LB21b                           SY × HL
   LB22     (AL|HL|EX|ID|EB|EM|IN|NU) × IN
   LB23                       (AL|HL) × NU
                                   NU × (AL|HL)
   LB23a                           PR × (ID|EB|EM)
                           (ID|EB|EM) × PO
   LB24                       (PR|PO) × (AL|HL)
                              (AL|HL) × (PR|PO)
   LB25                    (CL|CP|NU) × (PO|PR)
                              (PO|PR) × OP
                  (PO|PR|HY|IS|NU|SY) × NU
   LB26                            JL × (JL|JV|H2|H3)
                              (JV|H2) × (JV|JT)
                              (JT|H3) × JT
   LB27              (JL|JV|JT|H2|H3) × (IN|PO)
                                   PR × (JL|JV|JT|H2|H3)
   LB28                       (AL|HL) × (AL|HL)
   LB29                            IS × (AL|HL)
   LB30                    (AL|HL|NU) × OP
                                   CP × (AL|HL|NU)
   LB30a              sot (RI RI)* RI × RI
                    [^RI] (RI RI)* RI × RI
   LB30b                           EB × EM
   LB31                           ALL ÷
                                      ÷ ALL

   [1]: http://www.unicode.org/reports/tr14/#Algorithm
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt

   Given the structure of the rules we keep a window of three line
   break property value slots, two on the left, one on the right of a
   boundary and pattern match these slots to find the rule that
   applies. Because of LB9 these slots may actually correspond to more
   than one character and we need to bufferize the data for the slot
   on the right.
                            ---??--->
                     +----+----++----+
                  ...| l1 | l0 || r0 |
                     +----+----++----+
   already returned to client /  \ buffered in segmenter *)

type line =
  | AI | AL | B2 | BA | BB | BK | CB | CJ | CL | CM | CP
  | CR | EX | EB | EM | GL | H2 | H3 | HL | HY | ID | IN
  | IS | JL | JT | JV | LF | NL | NS | NU | OP | PO | PR
  | QU | RI | SA | SG | SP | SY | WJ | XX | ZW | ZWJ | Invalid | Sot | Eot

(* WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.line_break. *)

let byte_to_line =
  [| AL (* LB1 AI → AL *); AL; B2; BA; BB; BK; CB; NS (* LB1 CJ → NS *); CL;
     CM; CP; CR; EX; EB; EM; GL; H2; H3; HL; HY; ID; IN; IS; JL; JT; JV; LF;
     NL; NS; NU; OP; PO; PR; QU; RI; SA; AL (* LB1 SG → AL *); SP; SY; WJ;
     AL (* LB1 XX → AL *); ZW; ZWJ |]

let line u = match byte_to_line.(Uucp.Break.Low.line u) with
| SA ->
    begin match Uucp.Gc.general_category u with (* LB1 for SA *)
    | `Mn | `Mc -> CM
    | _ -> AL
    end
| l -> l

type state =
  | Fill                             (* fill slots on the right of boundary. *)
  | Flush                          (* flush slot r0 to get to next boundary. *)
  | Flush_SP                      (* special state to handle rules with SP*. *)
  | End                                                   (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    window : line array;                                    (* break window. *)
    mutable l0 : int;                            (* index in [window] of l0. *)
    r0_buf : Uuseg_buf.t;                                  (* buffer for r0. *)
    mutable odd_ri : bool;      (* odd number of RI on the left of boundary. *)
    mutable r0_is_zwj : bool;                         (* for LB8a r0 is ZWJ. *)
    mutable l0_is_zwj : bool;                         (* for LB8a l0 is ZWJ. *)
    mutable mandatory : bool; }             (* [true] if break is mandatory. *)

let u_dummy = `Uchar (Uchar.unsafe_of_int 0x0000)

let create () =
  { state = Fill;
    window = [|Invalid; Sot; Invalid; |];
    l0 = 1;
    r0_buf = Uuseg_buf.create 13;
    odd_ri = false;
    r0_is_zwj = false;
    l0_is_zwj = false;
    mandatory = false; }

let copy s =
  { s with window = Array.copy s.window;
           r0_buf = Uuseg_buf.copy s.r0_buf }

let l0_line s = s.window.(s.l0)
let r0_line s = s.window.((s.l0 + 1) mod Array.length s.window)
let r0_line_set s l =
  s.window.((s.l0 + 1) mod Array.length s.window) <- l;
  s.r0_is_zwj <- false

let r0_add s add = Uuseg_buf.add s.r0_buf add
let r0_empty s = Uuseg_buf.empty s.r0_buf
let r0_len s = Uuseg_buf.len s.r0_buf
let r0_flush s = Uuseg_buf.flush s.r0_buf
let window_move s =
  s.l0 <- (s.l0 + 1) mod Array.length s.window;
  if s.window.(s.l0) = RI then s.odd_ri <- not s.odd_ri else s.odd_ri <- false;
  s.l0_is_zwj <- s.r0_is_zwj;
  r0_line_set s Invalid

let decide s =
  let no_boundary s = r0_flush s in
  let wlen = Array.length s.window in
  let l0 = s.l0 in
  let r0 = (l0 + 1) mod wlen in
  let l1 = (l0 + 2) mod wlen in
  let w = s.window in
  s.mandatory <- false;
  match w.(l1), w.(l0) (**),(**) w.(r0) with
  (* LB1 is handled by [byte_to_line] and [line]. *)
  | (* LB2 *)   _, Sot, _ -> no_boundary s
  | (* LB3 is partly handled in [add]. *)
                _, _, Eot -> s.mandatory <- true; `Boundary
  | (* LB4 *)   _, BK, _ -> s.mandatory <- true; `Boundary
  | (* LB5 *)   _, CR, LF -> no_boundary s
  |             _, (CR|LF|NL), _ -> s.mandatory <- true; `Boundary
  | (* LB6 *)   _, _, (BK|CR|LF|NL) -> no_boundary s
  | (* LB7 is partly handled in [add] *)   _, _, (SP|ZW) -> no_boundary s
  | (* LB8 the SP* is handled in [add] *)  _, ZW, _ -> `Boundary
  | (* LB8a *) _, _, _ when s.l0_is_zwj -> no_boundary s
  (* LB9 is handled in [add]. *)
  (* LB10 is handled in [add]. *)
  | (* LB11 *)  _, _, WJ -> no_boundary s
  |             _, WJ, _ -> no_boundary s
  | (* LB12 *)  _, GL, _ -> no_boundary s
  | (* LB12a *) _, x, GL when x <> SP && x <> BA && x <> HY -> no_boundary s
  | (* LB13 *)  _, _, (CL|CP|EX|IS|SY) -> no_boundary s
  | (* LB14 the SP* is handled in [add] *) _, OP, _ -> no_boundary s
  | (* LB15 the SP* is handled in [add] *) _, QU, OP -> no_boundary s
  | (* LB16 the SP* is handled in [add] *) _, (CL|CP), NS -> no_boundary s
  | (* LB17 the SP* is handled in [add] *) _, B2, B2 -> no_boundary s
  | (* LB18 *)  _, SP, _ -> `Boundary
  | (* LB19 *)  _, _, QU -> no_boundary s
  |             _, QU, _ -> no_boundary s
  | (* LB20 *)  _, _, CB -> `Boundary
  |             _, CB, _ -> `Boundary
  | (* LB21 *)  _, _, (BA|HY|NS) -> no_boundary s
  |             _, BB, _ -> no_boundary s
  | (* LB21a *) HL, (BA|HY), _ -> no_boundary s
  | (* LB21b *) _, SY, HL -> no_boundary s
  | (* LB22 *)  _, (AL|HL|EX|ID|EB|EM|IN|NU), IN -> no_boundary s
  | (* LB23 *)  _, (AL|HL), NU -> no_boundary s
  |             _, NU, (AL|HL) -> no_boundary s
  | (* LB23a *) _, PR, (ID|EB|EM) -> no_boundary s
  |             _, (ID|EB|EM), PO -> no_boundary s
  | (* LB24 *)  _, (PR|PO), (AL|HL) -> no_boundary s
  |             _, (AL|HL), (PR|PO) -> no_boundary s
  | (* LB25 *)  _, (CL|CP|NU), (PO|PR) -> no_boundary s
  |             _, (PO|PR), OP -> no_boundary s
  |             _, (PO|PR|HY|IS|NU|SY), NU -> no_boundary s
  | (* LB26 *)  _, JL, (JL|JV|H2|H3) -> no_boundary s
  |             _, (JV|H2), (JV|JT) -> no_boundary s
  |             _, (JT|H3), JT -> no_boundary s
  | (* LB27 *)  _, (JL|JV|JT|H2|H3), (IN|PO) -> no_boundary s
  |             _, PR, (JL|JV|JT|H2|H3) -> no_boundary s
  | (* LB28 *)  _, (AL|HL), (AL|HL) -> no_boundary s
  | (* LB29 *)  _, IS, (AL|HL) -> no_boundary s
  | (* LB30 *)  _, (AL|HL|NU), OP -> no_boundary s
  |             _, CP, (AL|HL|NU) -> no_boundary s
  | (* LB30a *) _, RI, RI when s.odd_ri -> no_boundary s
  | (* LB30b *) _, EB, EM -> no_boundary s
  | (* LB31 *)  _, _, _ -> `Boundary

let flush_SP line r0_is_zwj add s =
  s.state <- Flush_SP; r0_line_set s line; s.r0_is_zwj <- r0_is_zwj;
  r0_add s add; r0_flush s

let add s = function
| `Uchar u as add ->
    begin match s.state with
    | Fill ->
        begin match line u with
        | (CM|ZWJ as b) ->
            if r0_line s = SP then flush_SP AL (b = ZWJ) add s (* LB10 *) else
            begin match l0_line s with
            | BK | CR | LF | NL | SP | ZW | Sot -> (* LB10 *)
                s.state <- Flush;
                r0_line_set s AL; s.r0_is_zwj <- (b = ZWJ);
                r0_add s add; decide s
            | _ ->
                s.l0_is_zwj <- (b = ZWJ);
                add (* LB9 *)
            end
        | SP ->
            if r0_line s = SP then ((* bufferize *) r0_add s add; `Await) else
            begin match l0_line s with
            | ZW -> add (* LB8's SP* *)
            | OP -> add (* LB14's SP* *)
            | (QU|CL|CP|B2) -> (* LB15, LB16, LB17 *)
                r0_line_set s SP; r0_add s add; `Await
            | _ ->
                s.state <- Flush; r0_line_set s SP; r0_add s add; decide s
            end
        | line ->
            if r0_line s <> SP
            then (s.state <- Flush; r0_line_set s line; r0_add s add; decide s)
            else begin match l0_line s, line with
            | QU, OP (* LB15 *) | (CL|CP), NS (* LB16 *) | B2, B2 (* LB17 *) ->
                s.state <- Flush; r0_line_set s line; r0_add s add; r0_flush s
            | _ -> flush_SP line false add s
            end
        end
    | Flush | Flush_SP -> Uuseg_base.err_exp_await add
    | End -> Uuseg_base.err_ended add
    end
| `Await ->
    begin match s.state with
    | Flush ->
        if not (r0_empty s) then r0_flush s else
        (s.state <- Fill; window_move s; `Await)
    | Flush_SP ->
        if r0_len s > 1 then ((* LB7 *) r0_flush s) else
        (* last one is not a SP, decide SP ? last  *)
        let last = r0_line s in
        r0_line_set s SP; window_move s; r0_line_set s last;
        (s.state <- if last = Eot then End else Flush); decide s
    | End -> `End
    | Fill -> `Await
    end
| `End ->
    begin match s.state with
    | Fill ->
        s.state <- End;
        if s.window.(s.l0) = Sot then `End (* LB2 on empty seq. *) else
        if r0_line s = SP then flush_SP Eot false u_dummy s
        else (s.mandatory <- true; `Boundary) (* LB3 *)
    | Flush | Flush_SP -> Uuseg_base.err_exp_await `End
    | End -> Uuseg_base.err_ended `End
    end

let mandatory s = s.mandatory

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
