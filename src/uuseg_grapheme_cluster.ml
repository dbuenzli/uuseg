(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
