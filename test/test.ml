(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Checks that Uuseg passes Unicode's Segmentation and Line break conformance
   tests and also performs other tests. *)

let str = Format.asprintf
let log f = Format.eprintf (f ^^ "@?")
let fail fmt =
  let fail _ = failwith (Format.flush_str_formatter ()) in
  Format.kfprintf fail Format.str_formatter fmt

let split_string s sep =                                          (* Grrrr. *)
  let rec split accum j =
    let i = try (String.rindex_from s j sep) with Not_found -> -1 in
    if (i = -1) then
      let p = String.sub s 0 (j + 1) in
      if p <> "" then p :: accum else accum
    else
    let p = String.sub s (i + 1) (j - i) in
    let accum' = if p <> "" then p :: accum else accum in
    split accum' (i - 1)
  in
  split [] (String.length s - 1)

let stack_to_loc stack =                                         (* Grrrrr. *)
  let stack = Printexc.raw_backtrace_to_string stack in
  try
    let start = String.index stack '\n' in
    let fstart = String.index_from stack start '\"' + 1 in
    let fend = String.rindex stack '\"' - 1 in
    let file = String.sub stack fstart (fend - fstart + 1) in
    let lstart = fend + 9 in
    let lend = String.rindex stack ',' - 1 in
    let line = String.sub stack lstart (lend - lstart + 1) in
    str "%s:%d: " file (int_of_string line)
  with
  | Not_found | Failure _ -> "??:"

let rec pp_spec ppf = function
| [] -> ()
| `B :: spec -> Format.fprintf ppf "%s" "÷ "; pp_spec ppf spec
| `U u :: spec -> Format.fprintf ppf "%04X " u; pp_spec ppf spec

let pp_boundary ppf = function
| `Grapheme_cluster -> Format.fprintf ppf "grapheme cluster:"
| `Word -> Format.fprintf ppf "word:"
| `Sentence -> Format.fprintf ppf "sentence:"
| `Line_break -> Format.fprintf ppf "line break:"

let test_case = ref 0
let fail = ref 0

let test seg src spec =
  incr test_case;
  let loc = Printexc.get_callstack 2 in
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
  if not (!ended) then begin
    incr fail;
    Format.printf "@.%s%a did not finish with `End."
      (stack_to_loc loc) pp_boundary seg
  end else begin
    if nseq = spec then () else
    begin
      incr fail;
      Format.printf "@.%s%a mismatch:@.impl: %a@.spec: %a@."
        (stack_to_loc loc) pp_boundary seg pp_spec nseq pp_spec spec
    end
  end

let test_string_list l spec =
  let rec pp_list ppf = function
  | [] -> ()
  | s :: ss -> Format.fprintf ppf "%S;@ " s; pp_list ppf ss
  in
  let loc = Printexc.get_callstack 2 in
  incr test_case;
  if l = spec then () else
  begin
    incr fail;
    Format.printf "@.%s mismatch:@.impl: @[[%a]@]@.spec: @[[%a]@]@."
      (stack_to_loc loc) pp_list l pp_list spec
  end

(* Conformance data decoding *)

let cp_of_string v =                           (* parses a code point value. *)
  let is_hex c = (0x30 <= c && c <= 0x39) || (0x41 <= c && c <= 0x46) in
  let cp = ref 0 in
  for k = 0 to (String.length v) - 1 do
    let c = Char.code v.[k] in
    if not (is_hex c) then (failwith v) else
    cp := !cp * 16 + (if c <= 0x39 then c - 48 else c - 55)
  done;
  !cp

let decode_conformance_specs ignores ic =
  let rec loop specs =
    match try Some (input_line ic) with End_of_file -> None with
    | None -> List.rev specs
    | Some l ->
        if String.length l > 0 && l.[0] = '#' then loop specs else
        try begin match split_string l '#' with
        | [comment] -> loop specs
        | test :: comment ->
            let spec = split_string test ' ' in
            begin try
              let rec to_spec acc = function
              | ( "\xC3\x97" (* × *) | "\xC3\x97\t" ) :: rest ->
                  to_spec acc rest
              | ( "\xC3\xB7" (* ÷ *) | "\xC3\xB7\t") :: rest ->
                  to_spec (`B :: acc) rest
              | uchar :: rest ->
                  let u = cp_of_string uchar in
                  if not (Uuseg.is_uchar u) then raise Exit else
                  to_spec (`U (cp_of_string uchar) :: acc) rest
              | [] ->
                  List.rev acc
              in
              let spec = to_spec [] spec in
              if ignores = [] then loop (spec :: specs) else
              try
                let reason = List.assoc spec ignores in
                log "Skip test (%s): %s\n" reason test;
                loop (specs)
              with
              | Not_found -> loop (spec :: specs)
              with Exit ->
                  log "Skip test (surrogate not a scalar value): %s\n" test;
                  loop specs
            end
        | [] -> failwith ""
        end
        with Failure f ->
          log "FAILURE: `%s'" f;
          log "Unable to parse line:\n`%s'\n" l; incr fail;
          loop specs
  in
  loop []

let line_break_ignores =
  (* Conformance tests of line breaking algorithm implement a tailoring
     that we don't implement. Here are the tests break according
     to that tailoring. *)
  [[`U 0x007D; `B; `U 0x0025; `B], "tailoring, violates LB25.1";
   [`U 0x007D; `U 0x0308; `B; `U 0x0025; `B], "tailoring, violates LB25.1";
   [`U 0x007D; `B; `U 0x0024; `B], "tailoring, violates LB25.3";
   [`U 0x007D; `U 0x0308; `B; `U 0x0024; `B], "tailoring, violates LB24.3";
   (* *)
   [`U 0x0029; `B; `U 0x0025; `B], "tailoring, violates LB25.2";
   [`U 0x0029; `U 0x0308; `B; `U 0x0025; `B], "tailoring, violates LB25.2";
   [`U 0x0029; `B; `U 0x0024; `B], "tailoring, violates LB25.4";
   [`U 0x0029; `U 0x0308; `B; `U 0x0024; `B], "tailoring, violates LB24.4";
   (* *)
   [`U 0x002C; `B; `U 0x0030; `B], "tailoring, violates LB25.12";
   [`U 0x002C; `U 0x0308; `B; `U 0x0030; `B], "tailoring, violates LB25.12";
   (* *)
   [`U 0x0025; `B; `U 0x0028; `B], "tailoring, violates LB25.7";
   [`U 0x0025; `U 0x0308; `B; `U 0x0028; `B], "tailoring, violates LB25.7";
   (* *)
   [`U 0x0024; `B; `U 0x0028; `B], "tailoring, violates LB25.9";
   [`U 0x0024; `U 0x0308; `B; `U 0x0028; `B], "tailoring, violates LB25.9";
   (* *)
   [`U 0x002F; `B; `U 0x0030; `B], "tailoring, violates LB25.14";
   [`U 0x002F; `U 0x0308; `B; `U 0x0030; `B], "tailoring, violates LB25.14";
   (* *)
   [ `U 0x0065; `U 0x0071; `U 0x0075; `U 0x0061; `U 0x006C; `U 0x0073;
     `U 0x0020; `U 0x002E; `B; `U 0x0033; `U 0x0035; `U 0x0020; `B; `U 0x0063;
     `U 0x0065; `U 0x006E; `U 0x0074; `U 0x0073; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0063; `U 0x006F; `U 0x0064; `U 0x0065; `B; `U 0x005C; `B;
     `U 0x0028; `U 0x0073; `B; `U 0x005C; `U 0x0029; `B ],
   "tailoring, violates LB25.9";
   [ `U 0x0063; `U 0x006F; `U 0x0064; `U 0x0065; `B; `U 0x005C; `B; `U 0x007B;
     `U 0x0073; `B; `U 0x005C; `U 0x007D; `B ],
   "tailoring, violates LB25.9";
   (* *)
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x0020; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x0020; `B; `U 0x672C; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x0020; `B; `U 0x0915; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x0020; `B; `U 0xBABB; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x3000; `B; `U 0x672C; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x3000; `B; `U 0x307E; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x3000; `B; `U 0x0033; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0041; `U 0x002E; `B; `U 0x0031; `U 0x0020; `B; `U 0xBABB; `B ],
   "tailoring, violates LB25.12";
   [ `U 0xBD24; `B; `U 0xC5B4; `U 0x002E; `U 0x0020; `B; `U 0x0041; `U 0x002E;
     `B; `U 0x0032; `U 0x0020; `B; `U 0xBCFC; `B ],
   "tailoring, violates LB25.12";
   [ `U 0xBD10; `B; `U 0xC694; `U 0x002E; `U 0x0020; `B; `U 0x0041; `U 0x002E;
     `B; `U 0x0033; `U 0x0020; `B; `U 0xBABB; `B ],
   "tailoring, violates LB25.12";
   [ `U 0xC694; `U 0x002E; `U 0x0020; `B; `U 0x0041; `U 0x002E; `B; `U 0x0034;
     `U 0x0020; `B; `U 0xBABB; `B ],
   "tailoring, violates LB25.12";
   [ `U 0x0061; `U 0x002E; `B; `U 0x0032; `U 0x3000; `B; `U 0x300C; `B ],
   "tailoring, violates LB25.12";
  ]

let rec seq_of_spec acc = function
| `U u :: rest -> seq_of_spec (u :: acc) rest
| `B :: rest -> seq_of_spec acc rest
| [] -> List.rev acc

let test_conformance seg name ignores inf =
  try
    log "Testing conformance of %s\n" name;
    let ic = open_in inf in
    let specs = decode_conformance_specs ignores ic in
    let test spec = test seg (seq_of_spec [] spec) spec in
    List.iter test specs;
    close_in ic
  with Sys_error e -> log "%s\n" e; incr fail

let test_others () =
  let g = `Grapheme_cluster in
  test g [] [`B;];
  test g [0x0020] [`B; `U 0x0020; `B;];
  test g (* éa *) [0x0065; 0x0301; 0x0061;]
    [`B; `U 0x0065; `U 0x0301; `B; `U 0x0061; `B;];
  let w = `Word in
  test w [] [`B;];
  let s = `Sentence in
  test s [] [];
  let l = `Line_break in
  test l [] [];
  ()

let test_uuseg_string () =
  let fold8 seg s =
    List.rev (Uuseg_string.fold_utf_8 seg (fun acc s -> s :: acc) [] s)
  in
  test_string_list (fold8 `Grapheme_cluster "") [];
  test_string_list (fold8 `Grapheme_cluster "ab cd") ["a"; "b"; " "; "c"; "d"];
  test_string_list (fold8 `Word "") [];
  test_string_list (fold8 `Word "ab cd") ["ab"; " "; "cd"];
  test_string_list (fold8 `Sentence "") [];
  test_string_list (fold8 `Sentence "ab cd") ["ab cd"];
  test_string_list (fold8 `Line_break "") [];
  test_string_list (fold8 `Line_break "ab cd") ["ab "; "cd"];
  ()

let test g_file w_file s_file l_file =
  try
    test_conformance `Grapheme_cluster "grapheme cluster boundary" [] g_file;
    test_conformance `Word "word boundary" [] w_file;
    test_conformance `Sentence "sentence boundary" [] s_file;
    test_conformance `Line_break "line break boundary" line_break_ignores
      l_file;
    log "Making other tests.\n";
    test_others ();
    log "Testing Uutf_string.\n";
    test_uuseg_string ();
    if !fail > 0
    then log "There %d FAILURES out of %d tests.\n" !fail !test_case
    else log "Success on %d tests!\n" !test_case
  with Sys_error e -> log "%s\n" e; exit 1

let main () =
  let usage = Printf.sprintf
    "Usage: %s [INFILE]\n\
    \ Runs the Unicode segmentation conformance test.\n\
    Options:" (Filename.basename Sys.executable_name)
  in
  let err _ = raise (Arg.Bad "no positional argument supported") in
  let g_file = ref "test/GraphemeBreakTest.txt" in
  let w_file = ref "test/WordBreakTest.txt" in
  let s_file = ref "test/SentenceBreakTest.txt" in
  let l_file = ref "test/LineBreakTest.txt" in
  let options =
    [ "-g", Arg.String (fun f -> g_file := f),
      "Specifies the GraphemeBreakTest.txt file";
      "-w", Arg.String (fun f -> w_file := f),
      "Specifies the WordBreakTest.txt file";
      "-s", Arg.String (fun f -> s_file := f),
      "Specifies the SentenceBreakTest.txt file";
      "-l", Arg.String (fun f -> l_file := f),
      "Specifies the LineBreakTest.txt file"; ]
  in
  Arg.parse (Arg.align options) err usage;
  test !g_file !w_file !s_file !l_file

let () = if (not !Sys.interactive) then main ()

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
