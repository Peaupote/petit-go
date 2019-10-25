type ident = string

and ty =
  | Tstruct of string
  | Tref    of string

type var = ty * ident

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Leq | Gt | Geq
  | And | Or

type unop  = Not | Deref | Ref

type expr =
  ENil
| Eident  of string
| Eint    of int64
| Estring of string
| Ebool   of bool
| Etuple  of expr list
| Eattr   of expr * ident
| Ecall   of ident * (expr list)
| Eunop   of unop * expr
| Ebinop  of binop * expr * expr

type side = Incr | Decr

type instruction =
  Inop
| Iexpr   of expr
| Iside   of expr * side
| Iasgn   of expr * expr
| Iblock  of instruction list
| Idecl   of ident * ty option
| Ireturn of expr
| Ifor    of expr * instruction
| Iif     of expr * instruction * instruction

type struct_t = {
    name : ident;
    body : var list }

type func_t = {
    name : ident;
    params : var list;
    return : ty list;
    body : instruction }

type decl =
  Dstruct of struct_t
| Dfunc   of func_t

type package = {
    name : string;
    imports : string list;
    structures : struct_t list;
    functions : func_t list }

let empty_pkg name imports = {
    name = name;
    imports = imports;
    structures = [];
    functions = [] }
