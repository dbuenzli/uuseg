(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Uuseg tests, including Unicode's Segmentation and Line break conformance
   tests. *)

open B0_testing

let pp_boundary ppf = function
| `Grapheme_cluster -> Format.fprintf ppf "Grapheme cluster"
| `Word -> Format.fprintf ppf "Word"
| `Sentence -> Format.fprintf ppf "Sentence"
| `Line_break -> Format.fprintf ppf "Line break"

let rec pp_spec ppf = function
| [] -> ()
| `B :: spec -> Format.fprintf ppf "÷ "; pp_spec ppf spec
| `U u :: spec ->
    Format.fprintf ppf "%04X " (Uchar.to_int u);
    (match spec with (`U _) :: _ -> Format.fprintf ppf "× " | _ -> ());
    pp_spec ppf spec

(* Conformance data decoding *)

let cp_of_string v = (* parses a code point value. *)
  let is_hex c = (0x30 <= c && c <= 0x39) || (0x41 <= c && c <= 0x46) in
  let cp = ref 0 in
  for k = 0 to (String.length v) - 1 do
    let c = Char.code v.[k] in
    if not (is_hex c) then failwith v else
    cp := !cp * 16 + (if c <= 0x39 then c - 48 else c - 55)
  done;
  !cp

let decode_conformance_specs ignores ic =
  let rec loop specs =
    match try Some (input_line ic) with End_of_file -> None with
    | None -> List.rev specs
    | Some l ->
        if String.length l > 0 && l.[0] = '#' then loop specs else
        try begin match String.split_on_char '#' l with
        | [comment] -> loop specs
        | test :: comment ->
            let spec = String.split_on_char ' ' test in
            begin try
              let rec to_spec acc = function
              | ( "\xC3\x97" (* × *) | "\xC3\x97\t" ) :: rest ->
                  to_spec acc rest
              | ( "\xC3\xB7" (* ÷ *) | "\xC3\xB7\t") :: rest ->
                  to_spec (`B :: acc) rest
              | uchar :: rest ->
                  let u = cp_of_string uchar in
                  if not (Uchar.is_valid u) then raise Exit else
                  to_spec (`U (Uchar.of_int u) :: acc) rest
              | [] ->
                  List.rev acc
              in
              let spec = to_spec [] spec in
              if ignores = [] then loop (spec :: specs) else
              try
                let reason = List.assoc spec ignores in
                Test.log "Skip test (%s): %s" reason test;
                loop (specs)
              with
              | Not_found -> loop (spec :: specs)
              with Exit ->
                Test.log
                  "Skip test (surrogate not a scalar value): %s" test;
                  loop specs
            end
        | [] -> failwith ""
        end
        with Failure f ->
          Test.fail "FAILURE: `%s'" f;
          Test.fail "Unable to parse line:\n`%s'\n" l;
          loop specs
  in
  loop []

let rec seq_of_spec acc = function
| `U u :: rest -> seq_of_spec (u :: acc) rest
| `B :: rest -> seq_of_spec acc rest
| [] -> List.rev acc

(* Conformance testing *)

let test_spec ?__POS__ seg src spec =
  let n = Uuseg.create seg in
  let ended = ref false in
  let rec add acc v = match Uuseg.add n v with
  | `Uchar u -> add ((`U u) :: acc) `Await
  | `Boundary -> add (`B :: acc) `Await
  | `Await -> ended := false; acc
  | `End -> ended := true; acc
  in
  let add_uchar acc u = add acc (`Uchar u) in
  let nseq = List.rev (add (List.fold_left add_uchar [] src) `End) in
  if not !ended then
    Test.fail ?__POS__ "%a did not finish with `End." pp_boundary seg else
  if nseq <> spec then
    Test.fail ?__POS__ "@[<v>%a mismatch:@,impl: %a@,spec: %a@]"
      pp_boundary seg pp_spec nseq pp_spec spec
  else Test.pass ()

let test_conformance seg name ignores inf =
  Test.test (Printf.sprintf "conformance of %s" name) @@ fun () ->
  let specs =
    try
      In_channel.with_open_bin inf @@ fun ic ->
      decode_conformance_specs ignores ic
    with Sys_error e -> Test.failstop "%s" e
  in
  let test spec = test_spec seg (seq_of_spec [] spec) spec in
  let fail ?__POS__ n ~checks =
    Test.log_fail "%a checks %a"
      Test.Fmt.count_ratio (n, checks) Test.Fmt.failed ()
  in
  Test.block ~__POS__ ~fail @@ fun () ->
  List.iter test specs

let test_others () =
  let u = Uchar.of_int in
  Test.test "other specifications" @@ fun () ->
  let g = `Grapheme_cluster in
  let test_spec ?__POS__ seg src spec =
    ignore (test_spec ?__POS__ seg src spec)
  in
  test_spec ~__POS__ g [] [];
  test_spec ~__POS__ g [u 0x0020] [`B; `U (u 0x0020); `B;];
  test_spec ~__POS__ g (* éa *) [u 0x0065; u 0x0301; u 0x0061;]
    [`B; `U (u 0x0065); `U (u 0x0301); `B; `U (u 0x0061); `B;];
  let w = `Word in
  test_spec ~__POS__ w [] [];
  let s = `Sentence in
  test_spec ~__POS__ s [] [];
  let l = `Line_break in
  test_spec ~__POS__ l [] [];
  ()

let test_uuseg_string () =
  Test.test "Uuseg_string" @@ fun () ->
  let rec pp_list ppf = function
  | [] -> ()
  | s :: ss -> Format.fprintf ppf "%S;@ " s; pp_list ppf ss
  in
  let fold8 seg s =
    List.rev (Uuseg_string.fold_utf_8 seg (fun acc s -> s :: acc) [] s)
  in
  let test ?__POS__ l spec =
    if l = spec then () else
    Test.fail ?__POS__
      "@[<v>Mismatch:@,impl: @[[%a]@]@,spec: @[[%a]@]" pp_list l pp_list spec
  in
  test ~__POS__ (fold8 `Grapheme_cluster "") [];
  test ~__POS__  (fold8 `Grapheme_cluster "ab cd") ["a"; "b"; " "; "c"; "d"];
  test ~__POS__ (fold8 `Word "") [];
  test ~__POS__ (fold8 `Word "ab cd") ["ab"; " "; "cd"];
  test ~__POS__ (fold8 `Sentence "") [];
  test ~__POS__ (fold8 `Sentence "ab cd") ["ab cd"];
  test ~__POS__ (fold8 `Line_break "") [];
  test ~__POS__ (fold8 `Line_break "ab cd") ["ab "; "cd"];
  ()

let test_LB30b_assumption () =
  Test.test "LB30b's data assumption" @@ fun () ->
  (* This is needed by our implementation of LB30b *)
  let rec loop u =
    let c = Uucp.Emoji.is_extended_pictographic u &&
            Uucp.Gc.general_category u = `Cn
    in
    if c then begin
      if Uucp.Break.line u = `ID ||
         Uucp.Break.line u = `XX (* Since Unicode 16.0.0 a few unassigned
                                    chars satisfy [c]. Those get transformed
                                    to AL *)
      then () else
      (Test.fail "LB30b assumption failure for U+%04X: %a"
         (Uchar.to_int u) Uucp.Break.pp_line (Uucp.Break.line u))
    end;
    if Uchar.equal u Uchar.max then () else loop (Uchar.succ u)
  in
  loop Uchar.min

let test g_file w_file s_file l_file =
  Test.main @@ fun () ->
  test_LB30b_assumption ();
  test_conformance `Grapheme_cluster "grapheme cluster boundary" [] g_file;
  test_conformance `Word "word boundary" [] w_file;
  test_conformance `Sentence "sentence boundary" [] s_file;
  test_conformance `Line_break "line break boundary" [] l_file;
  test_others ();
  test_uuseg_string ()

let main () =
  let usage = Printf.sprintf
      "Usage: %s [INFILE]\n\
      \ Runs the Unicode segmentation conformance tests.\n\
    Options:" (Filename.basename Sys.executable_name)
  in
  let err _ = raise (Arg.Bad "no positional argument supported") in
  let g_file = ref "test/GraphemeBreakTest.txt" in
  let w_file = ref "test/WordBreakTest.txt" in
  let s_file = ref "test/SentenceBreakTest.txt" in
  let l_file = ref "test/LineBreakTest.txt" in
  let str = Printf.sprintf in
  let options =
    [ "-g", Arg.String (fun f -> g_file := f),
      str "Specifies the GraphemeBreakTest.txt file. Defaults to %s" !g_file;
      "-w", Arg.String (fun f -> w_file := f),
      str "Specifies the WordBreakTest.txt file. Defaults to %s" !w_file;
      "-s", Arg.String (fun f -> s_file := f),
      str "Specifies the SentenceBreakTest.txt file. Defaults to %s" !s_file;
      "-l", Arg.String (fun f -> l_file := f),
      str "Specifies the LineBreakTest.txt file. Defaults to %s" !l_file]
  in
  Arg.parse (Arg.align options) err usage;
  test !g_file !w_file !s_file !l_file

let () = if !Sys.interactive then () else exit (main ())
