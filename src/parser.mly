%{
    open Ast
%}

%token EOF
%token BEGIN END LPAR RPAR SEMI COMMA EQ CEQ STAR DOT NOT
%token BEQ NEQ LT GT LEQ GEQ PLUS MINUS DIV MOD AND OR
%token ELSE IMPORT TRUE FALSE NIL TYPE AMP INCR DECR
%token FOR PACKAGE VAR FUNC RETURN IF STRUCT
%token <int> INT
%token <Ast.ident> IDENT
%token <string> STRING
%token <char> CHAR

%nonassoc IDENT
%nonassoc COMMA

%nonassoc MOD
%left DOT EQ LT LEQ GT GEQ NEQ
%right NOT AMP
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
  LPAR params = separated_list(COMMA, vars) RPAR
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
  BEGIN is = lblock(SEMI, END, instr) { is }
;

instr:
  s = instr_simple { s }
| b = block        { Iblock b }
| VAR ids = separated_nonempty_list(COMMA, IDENT) t = ty?
  vs = loption(rvalue)
    {
      let code =
        match vs with
        | [] -> List.fold_left (fun acc id -> (Idecl (id, t)) :: acc) [] ids
        | _  ->
           try
             List.fold_left2
               (fun acc id v ->
                 (Idecl (id, t)) :: (Iasgn (Eident id, v)) :: acc)
               [] ids vs
           with Invalid_argument _ -> raise Error
      in
      Iblock code
    }
;

instr_simple:
  e = expr { Iexpr e }
| e = expr PLUS PLUS { Iside (e, Incr) }
| e = expr MINUS MINUS { Iside (e, Decr) }
| e1 = separated_nonempty_list(COMMA, expr) e2 = rvalue
    {
      let code =
        try
          List.fold_left2
            (fun acc el er -> (Iasgn (el, er)) :: acc)
            [] e1 e2
        with Invalid_argument _ -> raise Error
      in
      Iblock code
    }
| ids = separated_nonempty_list(COMMA, IDENT)
  CEQ es = separated_nonempty_list(COMMA, expr)
    {
      let code =
        try
          List.fold_left2
            (fun acc id e -> (Iasgn ((Eident id), e)) :: acc)
            [] ids es
        with Invalid_argument _ -> raise Error
      in
      Iblock code
    }
;

rvalue:
  EQ es = separated_nonempty_list(COMMA, expr) { es }
;

expr:
  i = INT { Eint i }
| s = STRING { Estring s }
| i = IDENT { Eident i }
| TRUE { Ebool true }
| FALSE { Ebool false }
| NIL { ENil }
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
| EQ    { Eq }
| NEQ   { Neq }
| LT    { Lt }
| LEQ   { Leq }
| GT    { Gt }
| GEQ   { Geq }
;
