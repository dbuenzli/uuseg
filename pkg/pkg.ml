#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg


let distrib =
  (* FIXME OPAMv2, move this to an x-unicode-version field in the opam file. *)
  let watermarks = ("UNICODE_VERSION", `String "12.0.0") :: Pkg.watermarks in
  Pkg.distrib ~watermarks ()

let uutf = Conf.with_pkg "uutf"
let cmdliner = Conf.with_pkg "cmdliner"

let () =
  Pkg.describe "uuseg" ~distrib @@ fun c ->
  let uutf = Conf.value c uutf in
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib ~api:["Uuseg"] "src/uuseg.mllib";
       Pkg.mllib ~cond:uutf "src/uuseg_string.mllib";
       Pkg.bin ~cond:(uutf && cmdliner) "test/usegtrip";
       Pkg.test "test/test";
       Pkg.test "test/examples";
       Pkg.doc "test/examples.ml"; ]
