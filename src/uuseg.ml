(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


let unicode_version = Uucp.unicode_version

(* Segmenters *)

type 'a segmenter =
  { id : 'a Uuseg_base.Type.Id.t;
    name : string;
    create : unit -> 'a;
    copy : 'a -> 'a;
    equal : 'a -> 'a -> bool;
    mandatory : 'a -> bool;
    add : 'a -> [ `Uchar of Uchar.t | `Await | `End ] ->
      [ `Boundary | `Uchar of Uchar.t | `Await | `End ] }

type custom = C : 'a segmenter -> custom

type boundary =
  [ `Grapheme_cluster | `Word | `Sentence | `Line_break | `Custom of custom ]

let pp_boundary ppf b = match (b :> boundary) with
| `Grapheme_cluster -> Format.fprintf ppf "`Grapheme_cluster"
| `Word -> Format.fprintf ppf "`Word"
| `Sentence -> Format.fprintf ppf "`Sentence"
| `Line_break -> Format.fprintf ppf "`Line_break"
| `Custom (C s) -> Format.fprintf ppf "`Custom %s" s.name

(* Built-in segmenters *)

let mandatory_default _ = true

let grapheme_cluster =
  C { id = Uuseg_base.Type.Id.make ();
      name = "Uuseg.grapheme_cluster";
      create = Uuseg_grapheme_cluster.create;
      copy = Uuseg_grapheme_cluster.copy;
      equal = Uuseg_grapheme_cluster.equal;
      mandatory = mandatory_default;
      add = Uuseg_grapheme_cluster.add; }

let word =
  C { id = Uuseg_base.Type.Id.make ();
      name = "Uuseg.word";
      create = Uuseg_word.create;
      copy = Uuseg_word.copy;
      equal = Uuseg_word.equal;
      mandatory = mandatory_default;
      add = Uuseg_word.add; }

let sentence =
  C { id = Uuseg_base.Type.Id.make ();
      name = "Uuseg.sentence";
      create = Uuseg_sentence.create;
      copy = Uuseg_sentence.copy;
      equal = Uuseg_sentence.equal;
      mandatory = mandatory_default;
      add = Uuseg_sentence.add; }

let line_break =
  C { id = Uuseg_base.Type.Id.make ();
      name = "Uuseg.line_break";
      create = Uuseg_line_break.create;
      copy = Uuseg_line_break.copy;
      equal = Uuseg_line_break.equal;
      mandatory = Uuseg_line_break.mandatory;
      add = Uuseg_line_break.add; }

(* Generic segmenter inteface *)

type t = Seg : boundary * 'a * 'a segmenter -> t
type ret = Uuseg_base.ret

let create boundary =
  let (C seg) = match boundary with
  | `Grapheme_cluster -> grapheme_cluster
  | `Word -> word
  | `Sentence -> sentence
  | `Line_break -> line_break
  | `Custom c -> c
  in
  Seg ((boundary :> boundary), seg.create (), seg)

let boundary (Seg (boundary, _, _)) = boundary
let add (Seg (_, s, seg)) add = seg.add s add
let mandatory (Seg (_, s, seg)) = seg.mandatory s
let copy (Seg (b, s, seg)) = Seg (b, seg.copy s, seg)

let equal (Seg (b0, s0, seg0)) (Seg (b1, s1, seg1)) = match b0, b1 with
| `Custom _, _ | _, `Custom _ ->
    invalid_arg "Cannot test custom segmenters for equality"
| _ ->
    match Uuseg_base.Type.Id.provably_equal seg0.id seg1.id with
    | None -> false
    | Some Uuseg_base.Type.Equal -> seg0.equal s0 s1

let pp_ret = Uuseg_base.pp_ret

(* Custom segmenters *)

let custom ?(mandatory = mandatory_default) ~name ~create ~copy ~add () =
  (* N.B. when we require > 5.1 we can replace Uuseg_base.Type by
     Stdlib.Type and open up equality testing for custom *)
  let id = Uuseg_base.Type.Id.make () in
  let equal _ _ = assert false in
  C { id; name; create; copy; equal; mandatory; add }

let err_exp_await = Uuseg_base.err_exp_await
let err_ended = Uuseg_base.err_ended
