type ident = string

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Leq | Gt | Geq
  | And | Or

type unop  = Not | Deref | Ref
type side = Incr | Decr
type position = Lexing.position * Lexing.position

type 'a loc =
  { v : 'a;
    position : position }

and ty =
  | Tstruct of string loc
  | Tref    of string loc

and var = ty loc * ident loc

and expr =
  Enil
| Eident  of string
| Eint    of int64
| Estring of string
| Ebool   of bool
| Etuple  of expr loc list
| Eattr   of expr loc * ident loc
| Ecall   of expr loc * (expr loc list)
| Eunop   of unop * expr loc
| Ebinop  of binop * expr loc * expr loc

and instruction =
  Inop
| Iexpr   of expr loc
| Iside   of expr loc * side
| Iasgn   of expr loc * expr loc
| Iblock  of instruction list
| Idecl   of ident loc list * ty loc option * expr loc option
| Ireturn of expr loc
| Ifor    of expr loc * instruction
| Iif     of expr loc * instruction * instruction

and struct_t = {
    s_name : ident loc;
    s_body : var list; }

and func_t = {
    f_name   : ident loc;
    f_params : var list;
    f_return : ty loc list;
    f_body   : instruction; }

and decl =
  Dstruct of struct_t loc
| Dfunc   of func_t loc

type package = {
    p_name       : string loc;
    p_imports    : string loc list;
    p_structures : struct_t loc list;
    p_functions  : func_t loc list }

let empty_pkg name imports = {
    p_name       = name;
    p_imports    = imports;
    p_structures = [];
    p_functions  = [] }

module Prog = Map.Make(struct type t = string
                              let compare = compare end)
