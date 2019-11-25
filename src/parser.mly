%{
    open Ast

    let tuple lst =
      match lst with
      | [] -> assert false
      | x :: [] -> x
      | _ ->
         let hd = List.hd lst in
         let tl = List.tl lst in
         let pos =
           List.fold_left
             (fun (s, _) x -> (s, snd x.position))
             (fst hd.position, snd hd.position) tl
         in
         { v = Etuple lst; position = pos }

%}

%token EOF
%token BEGIN END LPAR RPAR SEMI COMMA EQ CEQ STAR DOT NOT
%token BEQ NEQ LT GT LEQ GEQ PLUS MINUS DIV MOD AND OR
%token ELSE IMPORT TRUE FALSE NIL TYPE AMP INCR DECR
%token FOR PACKAGE VAR FUNC RETURN IF STRUCT
%token <int64> INT
%token <Ast.ident> IDENT
%token <string> STRING

%right AND OR
%left BEQ LT LEQ GT GEQ NEQ

%left PLUS MINUS
%left STAR DIV
%nonassoc MOD

%right NOT AMP
%left DOT
%nonassoc uminus

%type <Ast.package> package
%start package

%%

lblock(sep, next, X):
  next                               { [] }
| x = X next                         { [x] }
| x = X sep e = lblock(sep, next, X) { x :: e }
;

loc(X):
  x = X { { v = x ; position = $startpos, $endpos } }
;

package:
  PACKAGE name = loc(IDENT) SEMI
  imps = imports*
  d = decl*
  EOF
    {
      List.fold_left
        (fun prog d ->
          match d with
          | Dstruct s -> { prog with p_structures = s :: prog.p_structures }
          | Dfunc f -> { prog with p_functions = f :: prog.p_functions })
        (empty_pkg name imps) d
    }
;

imports:
  IMPORT pkg = loc(STRING) SEMI { pkg }
;

decl:
  s = loc(structure) { Dstruct s }
| f = loc(func)      { Dfunc   f }
;

structure:
  TYPE name = loc(IDENT) STRUCT BEGIN
  vs = lblock(SEMI, END, vars) SEMI
    { { s_name = name; s_body = List.concat vs } }
;

func:
  FUNC name = loc(IDENT)
  LPAR params = lblock(COMMA, RPAR, vars)
  ret = loption(return_ty)
  body = block SEMI
    { { f_name = name;
        f_params = List.concat params;
        f_return = ret;
        f_body = body } }
;

return_ty:
  t = loc(ty) { [t] }
| LPAR ts = lblock(COMMA, RPAR, loc(ty)) { ts }
;

ty:
  s = loc(IDENT)      { Tstruct s }
| STAR s = loc(ty) { Tref s }
;

vars:
  ids = separated_nonempty_list(COMMA, loc(IDENT)) t = loc(ty)
    { List.fold_left (fun acc id -> (t, id) :: acc) [] ids }
;

in_block:
  END { [] }
| i = instr END { i :: [] }
| SEMI next = in_block { next }
| i = instr SEMI next = in_block { i :: next }
;

block:
  BEGIN is = in_block { Iblock is }
;

instr:
| s = instr_simple { s }
| i = instr_if     { i }
| b = block        { b }
| VAR ids = separated_nonempty_list(COMMA, loc(IDENT)) t = loc(ty)?
  vs = rvalue? { Idecl (ids, t, vs) }
| FOR b = block { Ifor ({ v = Ebool true; position = $startpos, $endpos }, b) }
| FOR e = loc(expr) b = block { Ifor (e, b) }
| FOR i1 = instr_simple? SEMI
      e  = loc(expr) SEMI
      i2 = instr_simple?
      b = block
            { match i1, i2 with
              | None, None -> Ifor (e, b)
              | Some i, None -> Iblock [i; Ifor (e, b)]
              | None, Some i -> Ifor (e, Iblock [b; i])
              | Some i1, Some i2 -> Iblock [i1; Ifor (e, Iblock [b; i2])] }
;

instr_simple:
  e = loc(expr) { Iexpr e }
| e = loc(expr) INCR { Iside (e, Incr) }
| e = loc(expr) DECR { Iside (e, Decr) }
| e1 = separated_nonempty_list(COMMA, loc(expr)) e2 = rvalue
      { Iasgn (tuple e1, e2) }
| ids = separated_nonempty_list(COMMA, loc(expr))
  CEQ vs = separated_nonempty_list(COMMA, loc(expr))
             { let ids =
                 List.map
                   (fun id ->
                     match id.v with
                     | Eident i -> { id with v = i}
                     | _ -> raise Parsing.Parse_error)
                 ids
               in
               Idecl (ids, None, Some (tuple vs)) }
| RETURN { Ireturn { v = Enil; position = $startpos, $endpos } }
| RETURN es = separated_nonempty_list(COMMA, loc(expr)) { Ireturn (tuple es) }
;

instr_if:
  IF e = loc(expr) b = block { Iif (e, b, Inop) }
| IF e = loc(expr) b1 = block ELSE b2 = instr_if { Iif (e, b1, b2) }
| IF e = loc(expr) b1 = block ELSE b2 = block { Iif (e, b1, b2) }
;

rvalue:
  EQ es = separated_nonempty_list(COMMA, loc(expr)) { tuple es }
;

expr:
  i = INT     { Eint i }
| s = STRING  { Estring s }
| i = IDENT   { Eident i }
| TRUE        { Ebool true }
| FALSE       { Ebool false }
| NIL         { Enil }
| LPAR e = expr RPAR { e }
| e = loc(expr) DOT a = after_dot
      { match a with
        | i, None -> Eattr (e, i)
        | i, Some es -> Ecall (Some i, i, es) }
| i = loc(IDENT) LPAR es = separated_list(COMMA, loc(expr)) RPAR
              { Ecall (None, i, es) }
| e1 = loc(expr) o = binop e2 = loc(expr) { Ebinop (o, e1, e2) }
| o = unop e = loc(expr) { Eunop(o, e) }
| MINUS e = loc(expr) %prec uminus
  { Ebinop(Sub, {v = Eint Int64.zero;
                 position = $startpos, $endpos}, e) }
;

after_dot:
  i = loc(IDENT) { i, None }
| i = loc(IDENT) LPAR es = separated_list(COMMA, loc(expr)) RPAR
                 { i, Some es }
;

%inline unop:
  NOT  { Not }
| AMP  { Ref }
| STAR { Deref }
;

%inline binop:
  PLUS  { Add }
| MINUS { Sub }
| STAR  { Mul }
| DIV   { Div }
| MOD   { Mod }
| BEQ   { Eq  }
| NEQ   { Neq }
| LT    { Lt  }
| LEQ   { Leq }
| GT    { Gt  }
| GEQ   { Geq }
| AND   { And }
| OR    { Or  }
;
