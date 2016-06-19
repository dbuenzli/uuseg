(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* These are the rules as found in [1], with property values aliases [2]
   substituted.

   GB1.        sot ÷
   GB2.            ÷ eot
   GB3.         CR × LF
   GB4. (CN|CR|LF) ÷
   GB5.            ÷ (CN|CR|LF)
   GB6.          L × (L|V|LV|LVT)
   GB7.     (LV|V) × (V|T)
   GB8.    (LVT|T) × T
   GB8a.        RI × RI
   GB9.            × EX
   GB9a.           × SM
   GB9b.        PP ×
   GB10.       Any ÷ Any

   [1]: http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries
   [2]: http://www.unicode.org/Public/7.0.0/ucd/PropertyValueAliases.txt
   [3]: http://www.unicode.org/Public/7.0.0/ucd/auxiliary/GraphemeBreakTest.html

   By the structure of the rules we see that grapheme clusters
   boundaries can be determined by simply looking at the grapheme
   cluster break property value of the character on the left and on
   the right of a boundary. The table below expresses this, it was
   derived manually from the rules and cross-checked with [3]. Rows
   are the left hand side of the rule (i.e. left character) and
   columns the right hand side (i.e. right character). GB2 and GB1
   on the empty sequence is handled in code.

   WARNING. The indexes used here need to be synchronized with those
   assigned by uucp for Uucp.Break.Low.grapheme_cluster. *)

type break_prop = int
let sot = Uucp.Break.Low.grapheme_cluster_max + 1
let eot = sot + 1

let break =
  let   o = true in  (* break *)
  let __X = false in (* no break *)
         [| (* CN; CR; EX;  L; LF; LV;LVT; PP; RI; SM;  T;  V; XX;*)
  (* CN *)  [|  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;|];
  (* CR *)  [|  o;  o;  o;  o;__X;  o;  o;  o;  o;  o;  o;  o;  o;|];
  (* EX *)  [|  o;  o;__X;  o;  o;  o;  o;  o;  o;__X;  o;  o;  o;|];
  (* L  *)  [|  o;  o;__X;__X;  o;__X;__X;  o;  o;__X;  o;__X;  o;|];
  (* LF *)  [|  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;|];
  (* LV *)  [|  o;  o;__X;  o;  o;  o;  o;  o;  o;__X;__X;__X;  o;|];
  (* LVT *) [|  o;  o;__X;  o;  o;  o;  o;  o;  o;__X;__X;  o;  o;|];
  (* PP *)  [|__X;__X;__X;__X;__X;__X;__X;__X;__X;__X;__X;__X;__X;|];
  (* RI *)  [|  o;  o;__X;  o;  o;  o;  o;  o;__X;__X;  o;  o;  o;|];
  (* SM *)  [|  o;  o;__X;  o;  o;  o;  o;  o;  o;__X;  o;  o;  o;|];
  (* T *)   [|  o;  o;__X;  o;  o;  o;  o;  o;  o;__X;__X;  o;  o;|];
  (* V *)   [|  o;  o;__X;  o;  o;  o;  o;  o;  o;__X;__X;__X;  o;|];
  (* XX *)  [|  o;  o;__X;  o;  o;  o;  o;  o;  o;__X;  o;  o;  o;|];
  (* sot *) [|  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;  o;|]|]

type state =
  | Fill  (* get next uchar to decide boundary. *)
  | Flush (* an uchar is buffered, client needs to get it out with `Await. *)
  | End   (* `End was added. *)

type t =
  { mutable state : state;                                 (* current state. *)
    mutable left : break_prop;     (* break property value left of boundary. *)
    mutable buf : [ `Uchar of int ] }                     (* bufferized add. *)

let create () = { state = Fill; left = sot; buf = `Uchar 0 (* overwritten *); }
let copy s = { s with state = s.state; }
let add s = function
| `Uchar u as add ->
    begin match s.state with
    | Fill ->
        let right = Uucp.Break.Low.grapheme_cluster u in
        if not break.(s.left).(right) then (s.left <- right; add) else
        (s.left <- right; s.state <- Flush; s.buf <- add; `Boundary)
    | Flush -> Uuseg_base.err_exp_await add
    | End -> Uuseg_base.err_ended add
    end
| `Await ->
    begin match s.state with
    | Flush ->
        s.state <- Fill; (s.buf :> Uuseg_base.ret)
    | End -> `End
    | Fill -> `Await
    end
| `End ->
    begin match s.state with
    | Fill -> s.state <- End; `Boundary  (* GB2 and GB1 on empty sequence *)
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
