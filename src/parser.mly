%{
    open Ast

    let tuple lst =
      match lst with
      | x :: [] -> x
      | _ -> Etuple lst

%}

%token EOF
%token BEGIN END LPAR RPAR SEMI COMMA EQ CEQ STAR DOT NOT
%token BEQ NEQ LT GT LEQ GEQ PLUS MINUS DIV MOD AND OR
%token ELSE IMPORT TRUE FALSE NIL TYPE AMP INCR DECR
%token FOR PACKAGE VAR FUNC RETURN IF STRUCT
%token <int64> INT
%token <Ast.ident> IDENT
%token <string> STRING

%nonassoc COMMA
%nonassoc IDENT

%nonassoc MOD
%left DOT BEQ LT LEQ GT GEQ NEQ
%right NOT AMP AND OR
%left PLUS MINUS
%left STAR DIV

%type <Ast.package> package
%start package

%%

lblock(sep, next, X):
  next                               { [] }
| x = X next                         { [x] }
| x = X sep e = lblock(sep, next, X) { x :: e }
;

package:
  PACKAGE name = IDENT SEMI
  imps = imports*
  d = decl*
  EOF
    {
      List.fold_left
        (fun prog d ->
          match d with
          | Dstruct s -> { prog with structures = s :: prog.structures }
          | Dfunc f -> { prog with functions = f :: prog.functions })
        (empty_pkg name imps) d
    }
;

imports:
  IMPORT pkg = STRING SEMI { pkg }
;

decl:
  s = structure { Dstruct s }
| f = func      { Dfunc   f }
;

structure:
  TYPE name = IDENT STRUCT BEGIN
  vs = lblock(SEMI, END, vars) SEMI
    { { name = name; body = List.concat vs } }
;

func:
  FUNC name = IDENT
  LPAR params = lblock(COMMA, RPAR, vars)
  ret = loption(return_ty)
  body = block SEMI
    { { name = name;
        params = List.concat params;
        return = ret;
        body = body } }
;

return_ty:
  t = ty { [t] }
| LPAR ts = separated_nonempty_list(COMMA, ty) RPAR { ts }
;

ty:
  s = IDENT      { Tstruct s }
| STAR s = IDENT { Tref s }
;

vars:
  ids = separated_nonempty_list(COMMA, IDENT) t = ty
    { List.fold_left (fun acc id -> (t, id) :: acc) [] ids }
;

block:
  BEGIN is = lblock(SEMI, END, instr) { Iblock is }
;

instr:
  s = instr_simple { s }
| i = instr_if     { i }
| b = block        { b }
| VAR ids = separated_nonempty_list(COMMA, IDENT) t = ty?
  vs = loption(rvalue)
    {
      let declarations = List.map (fun id -> Idecl (id, t)) ids in
      match vs with
      | [] -> Iblock declarations
      | _  ->
         let ids = List.map (fun id -> Eident id) ids in
         Iblock (declarations @ [Iasgn (Etuple ids, Etuple vs)])
    }
| FOR b = block { Ifor (Ebool true, b) }
| FOR e = expr b = block { Ifor (e, b) }
| FOR i1 = instr_simple? SEMI
      e  = expr SEMI
      i2 = instr_simple?
      b = block
            { match i1, i2 with
              | None, None -> Ifor (e, b)
              | Some i, None -> Iblock [i; Ifor (e, b)]
              | None, Some i -> Ifor (e, Iblock [b; i])
              | Some i1, Some i2 -> Iblock [i1; Ifor (e, Iblock [b; i2])] }
;

instr_simple:
  e = expr { Iexpr e }
| e = expr INCR { Iside (e, Incr) }
| e = expr DECR { Iside (e, Decr) }
| e1 = separated_nonempty_list(COMMA, expr) e2 = rvalue
     { Iasgn (tuple e1, tuple e2) }

(* problem here *)
| ids = separated_nonempty_list(COMMA, IDENT)
  CEQ es = separated_nonempty_list(COMMA, expr)
    { let ident_lst = List.map (fun id -> Eident id) ids in
      Iasgn (tuple ident_lst, tuple es) }
| RETURN es = separated_list(COMMA, expr) { Ireturn (tuple es) }
;

instr_if:
  IF e = expr b = block { Iif (e, b, Inop) }
| IF e = expr b1 = block ELSE b2 = instr_if { Iif (e, b1, b2) }
| IF e = expr b1 = block ELSE b2 = block { Iif (e, b1, b2) }
;

rvalue:
  EQ es = separated_nonempty_list(COMMA, expr) { es }
;

expr:
  i = INT    { Eint i }
| s = STRING { Estring s }
| i = IDENT  { Eident i }
| TRUE       { Ebool true }
| FALSE      { Ebool false }
| NIL        { ENil }
| LPAR e = expr RPAR { e }
| e = expr DOT i = IDENT { Eattr (e, i) }
| f = IDENT LPAR es = separated_list(COMMA, expr) RPAR { Ecall (f, es) }
| o = unop e = expr { Eunop(o, e) }
| e1 = expr o = binop e2 = expr { Ebinop (o, e1, e2) }
;

%inline unop:
  NOT  { Not }
| AMP  { Deref }
| STAR { Ref }
;

%inline binop:
  PLUS  { Add }
| MINUS { Sub }
| STAR  { Mul }
| DIV   { Div }
| MOD   { Mod }
| BEQ   { Eq }
| NEQ   { Neq }
| LT    { Lt }
| LEQ   { Leq }
| GT    { Gt }
| GEQ   { Geq }
| AND   { And }
| OR    { Or }
;
