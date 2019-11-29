open Format
open Lexing
open Config
open Ast

exception Error of string
let error msg = raise (Error msg)

exception Compile_error of position * string
let compile_error p msg = raise (Compile_error (p, msg))

let nil_cmp_error p = compile_error p "you can't compare nil with nil"
let invalid_argument p op msg =
  compile_error p (sprintf "invalid argument for %s: %s" op msg)
let error_left_value p op = invalid_argument p op "has to be a left value"

let unknown_field p s f =
  compile_error p (sprintf "struct `%s` has no field `%s`" s f)

let unknown_pkg p pkg =
  compile_error p (sprintf "unknown package `%s`" pkg)

let unknown_func p func =
  compile_error p (sprintf "unknown function `%s`" func)

let unknown_var p v =
  compile_error p (sprintf "unknown variable `%s`" v)

let unknown_type p ty =
  compile_error p (sprintf "unknown type %s" ty)

let decl_nb_error p e g =
  compile_error p (sprintf "expecting %d values but got %d" e g)

let untyped_nil p =
  compile_error p "you must precise to which type nil is a reference here"

let untyped_decl p =
  compile_error p "you must precise type or value of those variables"

let asgn_not_left_value p = compile_error p "this is not a left value"
let return_drop p = compile_error p "you can't return `_`"

let unused_var p v =
  compile_error p (sprintf "unused variable `%s`" v)

let unused_pkg p pkg =
  compile_error p (sprintf "unused package `%s`" pkg)

exception Double_pos_error of position * position * string
let double_pos_error p1 p2 msg = raise (Double_pos_error (p1, p2, msg))

let already_declared p1 p2 v =
  double_pos_error p1 p2
    (sprintf "variable `%s` has already been declared in this block" v)

let redondant_param_name p1 p2 v =
  double_pos_error p1 p2
    (sprintf "parameter name `%s` is used more than once" v)

let redondant_field_name p1 p2 v =
  double_pos_error p1 p2 (sprintf "field name `%s` is used more than once" v)

let args_nb_error p1 p2 f e g =
  let msg =
    if e < g
    then "function `%s` expects only %d arguments but you gave %d" ^^ ""
    else "function `%s` expects %d arguments but you only gave %d"
  in
  double_pos_error p1 p2 (sprintf msg f e g)

let func_already_exists p1 p2 f =
  double_pos_error p1 p2
    (sprintf "a function with name `%s` already exists" f)

let struct_already_exists p1 p2 s =
  double_pos_error p1 p2
    (sprintf "a structure with name `%s` already exists" s)

exception Cycle_struct of string list
let cycle_struct lst = raise (Cycle_struct lst)

let underline off l s e =
  for _ = 1 to off + 3 do printf " " done;
  for i = 1 to l do
    if i >= s && i <= e then printf "^" else printf "-" done;
  printf "@."

let log10 x =
  let rec aux l x = if x <= 0 then l else aux (l+1) (x / 10) in
  aux 0 x

let print_lines ic ub ue =
  printf "@.";
  let off = log10 ue.pos_lnum in
  let print_line_number fmt l =
    let nb_spaces = off - log10(l) + 1 in
    for _ = 1 to nb_spaces do fprintf fmt " " done;
    fprintf fmt "%d" l
  in

  let cdots = ref false in
  let i = ref 0 in
  let sc = ub.pos_cnum - ub.pos_bol + 1 in
  let ec = max 1 (ue.pos_cnum - ue.pos_bol) in
  while !i <= ue.pos_lnum do
    incr i;
    try let line = input_line ic in
        if (!i >= ub.pos_lnum - 1 && !i <= ub.pos_lnum + 1) ||
             (!i >= ue.pos_lnum - 1 && !i <= ue.pos_lnum + 1)
        then begin printf "%a: %s@." print_line_number !i line;
                   if !i = ub.pos_lnum
                   then underline off (String.length line) sc ec;
             end
        else if !i > ub.pos_lnum + 1 && not !cdots
        then begin
            cdots := true;
            for _ = 1 to off + 3 do printf " " done;
            printf "// ...@."
          end
    with End_of_file -> ()
  done;

  printf "@."

let print_file_pos (s, e) =
  let chan = open_in !ifile in
  print_lines chan s e;
  close_in chan

let print_double_pos (s1, e1) (s2, e2) =
  (* TODO : dont print 2 times if the positions are close to each other *)
  let chan = open_in !ifile in
  print_lines chan s1 e1;
  seek_in chan 0;
  print_lines chan s2 e2;
  close_in chan

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:@." !ifile l (c-1) c

let loc (s, e) =
  let l = s.pos_lnum in
  let sc = s.pos_cnum - s.pos_bol + 1 in
  let ec = e.pos_cnum - e.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:@." !ifile l sc ec

let rec print_cycle = function
  | [] -> ()
  | x :: [] -> eprintf "> structure `%s` contains itself@." x
  | x :: y :: [] ->
     eprintf "> structure `%s` constains structure `%s`@." x y
  | x :: ((y :: _) as tl)  ->
     eprintf "> structure `%s` constains structure `%s`@." x y;
     print_cycle tl

let error_fmt = "Error: %s.@." ^^ ""

let exit_with_error buf = function
  | Cycle_struct cycle ->
     eprintf "Error: there is a cycle in your structures@.";
     print_cycle cycle;
     exit 1
  | Error msg ->
     eprintf error_fmt msg;
     exit 1
  | Double_pos_error (p1, p2, msg) ->
     loc p2;
     eprintf error_fmt msg;
     print_double_pos p1 p2;
     exit 1
  | Compile_error (p, msg) ->
     loc p;
     eprintf error_fmt msg;
     print_file_pos p;
     exit 1
  | Lexer.Lexing_error msg ->
     let pos = Lexing.lexeme_start_p buf in
     localisation pos;
     eprintf "Lexical error: %s.@." msg;
     print_file_pos (pos, pos);
     exit 1
  | Parser.Error | Parsing.Parse_error ->
     let pos = Lexing.lexeme_start_p buf in
     localisation pos;
     eprintf "Syntax error@.";
     print_file_pos (pos, pos);
     exit 1
  | _ ->
     eprintf "An error occured@.";
     exit 1
