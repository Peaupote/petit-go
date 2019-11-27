open Lexing
open Format

let parse_only = ref false
let type_only  = ref false
let verbose    = ref false

let ifile = ref ""
let ofile = ref ""

let options = [
    "--parse-only", Arg.Set parse_only, " Stop execution after parsing";
    "--type-only",  Arg.Set type_only,  " Stop execution after typing";
    "-v",           Arg.Set verbose,    " Verbose mode"
  ]

let usage = "usage: " ^ Sys.argv.(0) ^ " file"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c
