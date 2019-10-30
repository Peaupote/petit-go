open Format
open Ast

let rec fprintf_list f fmt lst =
  match lst with
  | [] -> ()
  | x :: [] -> fprintf fmt "%a" f x
  | x :: xs -> fprintf fmt "%a," f x;
              fprintf_list f fmt xs

let fprintf_option f fmt = function
  | Some s -> fprintf fmt "%a" f s
  | None -> ()

let fprintf_comment fmt txt =
  fprintf fmt "/* %s */\n" txt

let fprintf_type fmt = function
  | Tstruct name -> fprintf fmt "%s" name.v
  | Tref name    -> fprintf fmt "*%s" name.v

let fprintf_struct fmt s =
  fprintf fmt "type %s struct {\n" s.s_name.v;
  List.iter (fun (ty, id) -> fprintf fmt "%s %a;\n" id.v fprintf_type ty.v) s.s_body;
  fprintf fmt "};\n"

let fprintf_params fmt ps =
  List.iter (fun (ty, id) -> fprintf fmt "%s %a," id.v fprintf_type ty.v) ps

let fprintf_return fmt ret =
  let fprintf_lst fmt = List.iter (fun ty -> fprintf fmt "%a," fprintf_type ty.v) in
  fprintf fmt "(%a)" fprintf_lst ret

let fprintf_unop fmt = function
  | Not -> fprintf fmt "!"
  | Deref -> fprintf fmt "&"
  | Ref -> fprintf fmt "*"

let fprintf_binop fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Mod -> fprintf fmt "%%"
  | Eq  -> fprintf fmt "=="
  | Neq -> fprintf fmt "!="
  | Lt  -> fprintf fmt "<"
  | Leq -> fprintf fmt "<="
  | Gt  -> fprintf fmt ">"
  | Geq -> fprintf fmt ">="
  | And -> fprintf fmt "&&"
  | Or  -> fprintf fmt "||"

let rec fprintf_expr fmt = function
  | Enil -> fprintf fmt "nil"
  | Eident i -> fprintf fmt "%s" i
  | Eint i -> fprintf fmt "%Ld" i
  | Estring s -> fprintf fmt "\"%s\"" s
  | Ebool b -> fprintf fmt "%B" b
  | Etuple lst ->
     fprintf fmt "(%a)"
       (fprintf_list fprintf_expr)
       (List.map (fun e -> e.v) lst)
  | Eattr (e, a) -> fprintf fmt "[%a].%s" fprintf_expr e.v a.v
  | Ecall (e, ps) ->
     fprintf fmt "[%a](%a)"
       fprintf_expr e.v
       (fprintf_list fprintf_expr)
       (List.map (fun e -> e.v) ps)
  | Eunop (op, e) ->
     fprintf fmt "[%a %a]"
       fprintf_unop op
       fprintf_expr e.v
  | Ebinop (op, e1, e2) ->
     fprintf fmt "[%a %a %a]"
       fprintf_expr e1.v
       fprintf_binop op
       fprintf_expr e2.v

let fprintf_side fmt = function
  | Incr -> fprintf fmt "++"
  | Decr -> fprintf fmt "--"

let fprintf_str fmt s = fprintf fmt "%s" s.v

let fprintf_opt f fmt = function
  | Some x -> fprintf fmt "%a" f x
  | None   -> ()

let rec fprintf_instruction fmt = function
  | Inop -> fprintf fmt "{}"
  | Iexpr e -> fprintf fmt "%a" fprintf_expr e.v
  | Iside (e, s) -> fprintf fmt "%a%a" fprintf_expr e.v fprintf_side s
  | Iasgn (e1, e2) ->
     fprintf fmt "%a = %a"
       fprintf_expr e1.v
       fprintf_expr e2.v
  | Iblock b ->
     fprintf fmt "{\n";
     List.iter (fun i -> fprintf fmt "%a;\n" fprintf_instruction i) b;
     fprintf fmt "}"
  | Idecl (ids, ty, Some e) ->
     fprintf fmt "var %a %a = %a"
       (fprintf_list fprintf_str) ids
       (fprintf_opt (fun fmt x -> fprintf_type fmt x.v)) ty
       fprintf_expr e.v
  | Idecl (ids, ty, None) ->
     fprintf fmt "var %a %a"
       (fprintf_list fprintf_str) ids
       (fprintf_opt (fun fmt x -> fprintf_type fmt x.v)) ty
  | Ireturn e ->
     fprintf fmt "return %a" fprintf_expr e.v
  | Ifor (c, i) ->
     fprintf fmt "for %a %a" fprintf_expr c.v fprintf_instruction i
  | Iif (c, i1, i2) ->
     fprintf fmt "if %a %a else %a"
       fprintf_expr c.v
       fprintf_instruction i1
       fprintf_instruction i2


let fprintf_func fmt f =
  fprintf fmt "func %s(%a) %a %a;\n"
    f.f_name.v
    fprintf_params f.f_params
    fprintf_return f.f_return
    fprintf_instruction f.f_body

let fprintf_prog fmt prog =
  Prog.iter
    (fun pkg ast ->
      fprintf_comment fmt "Reversed compiled package.";
      fprintf fmt "package %s;\n\n" pkg;

      fprintf_comment fmt "All imports of the package";
      List.iter (fun i -> fprintf fmt "import \"%s\";\n" i.v) ast.p_imports;
      fprintf fmt "\n";

      fprintf_comment fmt "All structures of the package";
      List.iter (fun s -> fprintf fmt "%a\n" fprintf_struct s.v) ast.p_structures;

      fprintf_comment fmt "All functions of the package";
      List.iter (fun f -> fprintf fmt "%a\n" fprintf_func f.v) ast.p_functions;

      fprintf_comment fmt "Enf of package.")
    prog;
  fprintf fmt "@."

let save file prog =
  let ch = open_out file in
  Format.fprintf (Format.formatter_of_out_channel ch) "%a" fprintf_prog prog;
  close_out ch
