(** Main file **)

open Format
open Lexing
open Ast

let parse_only = ref false
let type_only  = ref false

let ifile = ref ""
let ofile = ref ""

let options = [
    "--parse-only", Arg.Set parse_only, " Stop execution after parsing";
    "--type-only",  Arg.Set type_only,  " Stop execution after typing"
  ]

let usage = "usage: " ^ Sys.argv.(0) ^ " file"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let packages = ref Prog.empty

let compile file =
  ifile := file;

  if not (Filename.check_suffix file ".go")
  then begin
      eprintf "File must have .go extention.@.";
      Arg.usage options usage;
      exit 1;
    end;

  let f = open_in file in
  let buf = Lexing.from_channel f in
  try
    let pkg = Parser.package Lexer.token buf in
    if !parse_only then exit 0;

    if Prog.exists (fun name _ -> name = pkg.p_name.v) !packages
    then begin
        eprintf "Package with name %s already exists.@." pkg.p_name.v;
        exit 1
      end;

    packages := Prog.add pkg.p_name.v pkg !packages;

    close_in f;

    Fmt.save "rev_compiled.go" !packages
  with
  | Lexer.Lexing_error msg ->
     localisation (Lexing.lexeme_start_p buf);
     eprintf "Lexical error: %s.@." msg;
     exit 1
  | Parser.Error | Parsing.Parse_error ->
     localisation (Lexing.lexeme_start_p buf);
     eprintf "Syntax error@.";
     exit 1

let () =
  Arg.parse options compile usage;

  if !ifile = ""
  then begin
      eprintf "No file to compile.@.";
      Arg.usage options usage;
      exit 1;
    end;
