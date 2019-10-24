(** Main file **)

open Format
open Lexing

let ifile = ref ""
let ofile = ref ""

let setfile f s = f := s

let options = []
let usage = "usage: " ^ Sys.argv.(0) ^ " file"

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
  Arg.parse options (setfile ifile) usage;

  if !ifile = ""
  then begin
      eprintf "No file to compile.@.";
      exit 1;
    end;

  if not (Filename.check_suffix !ifile ".go")
  then begin
      eprintf "File must have .go extention.@.";
      Arg.usage options usage;
      exit 1;
    end;

  let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  try
    let _ = Parser.package Lexer.token buf in
    close_in f;
  with
  | Lexer.Lexing_error msg ->
     localisation (Lexing.lexeme_start_p buf);
     eprintf "Lexical error: %s.@." msg;
     exit 1
  | Parser.Error ->
     localisation (Lexing.lexeme_start_p buf);
     eprintf "Syntax error@.";
     exit 1
