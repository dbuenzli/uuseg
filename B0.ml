open B0_kit.V000
open Result.Syntax

let unicode_version = 15, 0, 0, None (* Adjust on new releases *)
let next_major = let maj, _, _, _ = unicode_version in (maj + 1), 0, 0, None

(* OCaml library names *)

let uuseg = B0_ocaml.libname "uuseg"
let uuseg_string = B0_ocaml.libname "uuseg.string"

let uucp = B0_ocaml.libname "uucp"
let uutf = B0_ocaml.libname "uutf"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let uuseg_lib =
  let srcs = Fpath.[ `Dir (v "src");
                     `X (v "src/uuseg_string.ml");
                     `X (v "src/uuseg_string.mli"); ]
  in
  let requires = [uucp] in
  B0_ocaml.lib uuseg ~doc:"The uuseg library" ~srcs ~requires

let uuseg_string_lib =
  let srcs = Fpath.[ `File (v "src/uuseg_string.ml");
                     `File (v "src/uuseg_string.mli") ]
  in
  let requires = [uucp; uuseg; uutf] in
  B0_ocaml.lib uuseg_string ~doc:"The uuseg.string library" ~srcs ~requires

(* Tools *)

let usegtrip =
  let srcs = Fpath.[`File (v "test/usegtrip.ml")] in
  let requires = [cmdliner; uutf; uuseg] in
  B0_ocaml.exe "usegtrip" ~doc:"The usegtrip tool" ~srcs ~requires

(* Tests *)

let test =
  let srcs = Fpath.[`File (v "test/test.ml")] in
  (* FIXME b0, this is not so good. *)
  let scope_dir b u = Fut.return (B0_build.scope_dir b u) in
  let meta =
    B0_meta.(empty
             |> tag test
             |> add B0_unit.Action.exec_cwd scope_dir)
  in
  let requires = [ uucp; uuseg; uuseg_string ] in
  B0_ocaml.exe "test" ~doc:"Test segmentation" ~srcs ~meta ~requires

let examples =
  let srcs = Fpath.[`File (v "test/examples.ml")] in
  let meta = B0_meta.(empty |> tag test) in
  let requires = [uutf; uuseg] in
  B0_ocaml.exe "examples" ~doc:"Examples" ~srcs ~meta ~requires

(* Cmdlets *)

let test_uri kind =
  Fmt.str "http://www.unicode.org/Public/%s/ucd/auxiliary/%sBreakTest.txt"
    (String.of_version unicode_version) kind

let download_tests =
  B0_cmdlet.v "download-tests" ~doc:"Download the UCD break tests" @@
  fun env _args -> B0_cmdlet.exit_of_result @@
  let get kind =
    let test_uri = test_uri kind in
    let test_file = Fpath.v (Fmt.str "test/%sBreakTest.txt" kind) in
    let test_file = B0_cmdlet.in_scope_dir env test_file in
    let stdout = Os.Cmd.out_file ~force:true ~make_path:true test_file in
    let* curl = Os.Cmd.get Cmd.(atom "curl" % "-f" % "-#" % "-S") in
    Log.app (fun m -> m "Downloading %s" test_uri);
    Os.Cmd.run Cmd.(curl % test_uri) ~stdout
  in
  let* () = get "Line" in
  let* () = get "Grapheme" in
  let* () = get "Word" in
  let* () = get "Sentence" in
  Ok ()

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The uuseg programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/uuseg"
    |> add online_doc "https://erratique.ch/software/uuseg/doc/"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/uuseg.git"
    |> add issues "https://github.com/dbuenzli/uuseg/issues"
    |> add description_tags
      ["unicode"; "text"; "segmentation"; "org:erratique"]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
         "--with-uutf" "%{uutf:installed}%"
         "--with-cmdliner" "%{cmdliner:installed}%" ]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depopts [ "uutf", ""; "cmdliner", ""]
    |> add B0_opam.Meta.conflicts
      [ "uutf", {|< "1.0.0"|};
        "cmdliner", {|< "1.1.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.03.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uucp", Fmt.str {|>= "%s" & < "%s"|}
          (String.of_version unicode_version) (String.of_version next_major)
      ]

  in
  B0_pack.v "default" ~doc:"uuseg package" ~meta ~locked:true @@
  B0_unit.list ()
