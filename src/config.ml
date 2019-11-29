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

let loc (s, e) =
  let l = s.pos_lnum in
  let s = s.pos_cnum - s.pos_bol + 1 in
  let e = e.pos_cnum - e.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l s e

let dbg f =
  let ppf = if !verbose then std_formatter else
              make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
  fprintf ppf f
