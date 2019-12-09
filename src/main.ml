open Format
open Config
open Ast
open Typer
open Error
open Compiler

let compile_order = Queue.create ()

(** Functions that parse and type a single file **)
let type_package file =
  ifile := file;

  if not (Filename.check_suffix file ".go")
  then begin
      eprintf "File must have .go extention.@.";
      Arg.usage options usage;
      exit 1;
    end;

  dbg "Parsing %s.@." file;
  let f = open_in file in
  let buf = Lexing.from_channel f in
  try
    let pkg = Parser.package Lexer.token buf in
    if !parse_only
    then begin
        dbg "Parse-only flag is on. If you want to compile, or check types consider removing it.@.";
        close_in f;
        exit 0;
      end;

    if Smap.mem pkg.p_name.v !all_packages
    then error (sprintf "package %s has already been completed" pkg.p_name.v);

    let env = type_prog empty_env pkg in
    all_packages := Smap.add pkg.p_name.v env !all_packages;
    Queue.add pkg.p_name.v compile_order;

    close_in f;
    dbg "Done with %s.@." file;
  with e -> exit_with_error buf e

let () =
  Arg.parse options type_package usage;

  if !ifile = ""
  then begin
      eprintf "No file to compile.@.";
      Arg.usage options usage;
      exit 1;
    end;

  dbg "All files typed.@.";

  try
    try
      let main = Smap.find "main" !all_packages in
      let main_func =
        try Smap.find "main" main.funcs
        with Not_found -> error "missing function main"
      in

      if snd main_func <> []
      then error "function main should return nothing";

      if fst main_func <> []
      then error "function main shouldn't take any arguments";

      if !type_only
      then begin
          dbg "Type-only flag is on. If you want to compile, consider removing it.@.";
          exit 0;
        end;

      dbg "@.";
      compile_program compile_order

    with Not_found -> error "missing package main"
  with Error msg -> eprintf "Error: %s.@." msg; exit 1
