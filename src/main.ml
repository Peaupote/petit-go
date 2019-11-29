(** Main file **)

open Format
open Config
open Ast
open Typer

let envs = ref Smap.empty

let compile file =
  ifile := file;

  dbg "coucou %s" file;

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
     loc p;
     eprintf "Error: %s.@." msg;
     exit 1
  | Typing_error (p, msg) ->
     loc p;
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
        let main_func =
          try Smap.find "main" main.funcs
          with Not_found -> error "missing function main"
        in

        if snd main_func <> []
        then error "function main should return nothing";

        if fst main_func <> []
        then error "function main shouldn't take any arguments";

    with Not_found -> error "missing package main"
  with Error msg -> eprintf "Error: %s.@." msg; exit 1
