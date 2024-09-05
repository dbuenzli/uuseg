open B0_kit.V000
open Result.Syntax

let unicode_version = 16, 0, 0, None (* Adjust on new releases *)
let next_major = let maj, _, _, _ = unicode_version in (maj + 1), 0, 0, None

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let uucp = B0_ocaml.libname "uucp"
let uutf = B0_ocaml.libname "uutf"
let cmdliner = B0_ocaml.libname "cmdliner"

let uuseg = B0_ocaml.libname "uuseg"

(* Libraries *)

let uuseg_lib =
  let srcs = [ `Dir ~/"src" ] in
  let requires = [ uucp ] in
  B0_ocaml.lib uuseg ~doc:"The uuseg library" ~srcs ~requires

let uuseg_string_lib =
  let exports = [uuseg] in
  B0_ocaml.deprecated_lib ~exports (B0_ocaml.libname "uuseg.string")

(* Tools *)

let usegtrip =
  let srcs = [ `File ~/"test/usegtrip.ml" ] in
  let requires = [ cmdliner; uutf; uuseg ] in
  B0_ocaml.exe "usegtrip" ~public:true ~doc:"The usegtrip tool" ~srcs ~requires

(* Tests *)

let test =
  let srcs = [ `File ~/"test/test_uuseg.ml" ] in
  let meta =
    B0_meta.(empty |> tag test |> tag run |> ~~ B0_unit.Action.cwd `Scope_dir)
  in
  let requires = [ b0_std; uucp; uuseg ] in
  B0_ocaml.exe "test_uucp" ~doc:"Test segmentations" ~srcs ~meta ~requires

let examples =
  let srcs = [ `File ~/"test/examples.ml" ] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [ uuseg ] in
  B0_ocaml.exe "examples" ~doc:"Doc samples" ~srcs ~meta ~requires

(* Actions *)

let show_version =
  B0_unit.of_action "unicode-version" ~doc:"Show supported unicode version" @@
  fun _ _ ~args:_ ->
  Ok (Log.app (fun m -> m "%s" (String.of_version unicode_version)))

let test_url kind =
  Fmt.str "http://www.unicode.org/Public/%s/ucd/auxiliary/%sBreakTest.txt"
    (String.of_version unicode_version) kind

let download_tests =
  let doc = "Download the UCD break tests" in
  B0_unit.of_action "download-tests" ~doc @@ fun env _ ~args:_ ->
  let get kind =
    let test_url = test_url kind in
    let test_file = Fpath.v (Fmt.str "test/%sBreakTest.txt" kind) in
    let test_file = B0_env.in_scope_dir env test_file in
    (Log.app @@ fun m ->
     m "@[<v>Downloading %s@,to %a@]" test_url Fpath.pp test_file);
    B0_action_kit.fetch_url env test_url test_file
  in
  let tests = ["Line"; "Grapheme"; "Word"; "Sentence"] in
  List.iter_iter_on_error ~error:(Log.if_error ~use:()) get tests;
  Ok ()

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The uuseg programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/uuseg"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/uuseg/doc/"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/uuseg.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/uuseg/issues"
    |> ~~ B0_meta.description_tags
      ["unicode"; "text"; "segmentation"; "org:erratique"]
    |> B0_meta.tag B0_opam.tag
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
         "--with-uutf" "%{uutf:installed}%"
         "--with-cmdliner" "%{cmdliner:installed}%" ]]|}
    |> ~~ B0_opam.depopts [ "uutf", ""; "cmdliner", ""]
    |> ~~ B0_opam.conflicts
      [ "uutf", {|< "1.0.0"|};
        "cmdliner", {|< "1.1.0"|}]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uucp",
        Fmt.str {|>= "%s" & < "%s"|}
          (String.of_version unicode_version)
          (String.of_version next_major)]
  in
  B0_pack.make "default" ~doc:"uuseg package" ~meta ~locked:true @@
  B0_unit.list ()
