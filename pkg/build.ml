#!/usr/bin/env ocaml
#directory "pkg";;
#use "topkg.ml";;

let uutf = Env.bool "uutf"
let cmdliner = Env.bool "cmdliner"

let () =
  Pkg.describe "uuseg" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/uuseg";
    Pkg.lib ~cond:uutf ~exts:Exts.module_library "src/uuseg_string";
    Pkg.bin ~cond:(uutf && cmdliner) ~auto:true "test/usegtrip";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]
