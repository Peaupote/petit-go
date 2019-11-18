(** Main file **)

open Format
open Lexing
open Ast
open Typer

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

let envs = ref Smap.empty

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

    let env = try Smap.find pkg.p_name.v !envs
              with Not_found -> empty_env in

    envs := Smap.add pkg.p_name.v (type_prog env pkg) !envs;

    close_in f;
  with
  | Error msg ->
     eprintf "Error: %s@." msg;
     exit 1
  | Compile_error (p, msg) ->
     localisation (fst p);
     eprintf "Error: %s.@." msg;
     exit 1
  | Typing_error (p, msg) ->
     localisation (fst p);
     eprintf "Error: %s.@." msg;
     exit 1
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

  try
    try let main = Smap.find "main" !envs in
        if not (Smap.mem "main" main.funcs)
        then error "no function main";
    with Not_found -> error "no package main"
  with Error msg -> eprintf "Error: %s.@." msg; exit 1
