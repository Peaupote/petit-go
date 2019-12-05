open Format
open Config

type ident = string

type binop =
  | Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Leq | Gt | Geq
  | And | Or

type unop  = Not | Deref | Ref
type position = Lexing.position * Lexing.position

(** Types for the parser *)
type 'a loc =
  { v : 'a;
    position : position }

and ty =
  | Tystruct of string loc option * string loc
  | Tyref    of ty loc

and var = ty loc * ident loc

and expr =
  Enil
| Eident  of string
| Eint    of int64
| Estring of string
| Ebool   of bool
| Etuple  of expr loc list
| Eattr   of expr loc * ident loc
| Ecall   of (ident loc option) * ident loc * (expr loc list)
| Eunop   of unop * expr loc
| Ebinop  of binop * expr loc * expr loc

and instruction =
  Inop
| Iexpr   of expr loc
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


(** Types for the typer *)
type typ =
  Tvoid
| Tnil
| Tint
| Tbool
| Tstring
| Ttuple  of typ list
| Tstruct of ident
| Tref    of typ

(** A type with information about the variable it's the type of
    (ie. if it's a left value or not) *)
type etype = { t : typ; left : bool }
let ltyp t = { t = t; left = true }
let typ t  = { t = t; left = false }

let typ_eq t1 t2 =
  match t1, t2 with
  | Tnil, Tref _ | Tref _, Tnil -> true
  | _ -> t1 = t2

let typ_neq t1 t2 = not (typ_eq t1 t2)

(** Type for functions and structures *)
type tfunc = typ list * typ list
type tstruct = typ Smap.t

(** Pretty printing *)
let rec pp_product fmt = function
  | [] -> ()
  | x :: [] -> fprintf fmt "%a" pp_typ x
  | x :: xs -> fprintf fmt "%a * " pp_typ x; pp_product fmt xs

and pp_typ fmt = function
  | Tvoid -> fprintf fmt "void"
  | Tnil -> fprintf fmt "nil"
  | Tint -> fprintf fmt "int"
  | Tbool -> fprintf fmt "bool"
  | Tstring -> fprintf fmt "string"
  | Ttuple tps -> fprintf fmt "%a" pp_product tps
  | Tstruct s -> fprintf fmt "%s" s
  | Tref t -> fprintf fmt "*%a" pp_typ t


(** Typed AST generated by typing *)

type texpr =
  Tenil
| Teint    of int64
| Testring of string
| Tebool   of bool
| Tident   of ident
| Tetuple  of texpr list
| Tattr    of texpr * ident
| Tcall    of (ident option) * ident * texpr list
| Tunop    of unop * texpr
| Tbinop   of binop * texpr * texpr
| Tprint   of texpr list
| Tnew     of typ

type tinstruction =
  Tnop
| Texpr   of texpr
| Tasgn   of texpr * texpr
| Tblock  of tinstruction list
| Tdecl   of ident list * typ option * texpr option
| Treturn of texpr
| Tfor    of texpr * tinstruction
| Tif     of texpr * tinstruction * tinstruction


(** All information about the package
    that you want to keep after typing *)
type env = {
    structs : tstruct Smap.t;
    types : typ Smap.t;
    funcs : tfunc Smap.t;
    funcs_body : tinstruction Smap.t;
    vars  : typ Smap.t;
    packages : Vset.t }

let empty_env =
  { structs = Smap.empty;
    types = Smap.empty;
    funcs = Smap.empty;
    funcs_body = Smap.empty;
    vars = Smap.empty;
    packages = Vset.empty }

let add_env v t env = { env with vars = Smap.add v t env.vars }

(** All informations about the package
    you are not willing to keep after typing *)
type env_info = {
    used_pkg    : Vset.t;
    used_vars   : Vset.t;

    (* usefull for error handling : to locate
       already defined functions, structs of variables *)
    local_vars  : position Smap.t;
    pkg_pos     : position Smap.t;
    func_pos    : position Smap.t;
    struct_pos  : position Smap.t;

    (* Allows to check if the return type if the good one and
       if each branch returns something *)
    return_type : typ list;
    is_return   : bool }

let empty_info = {
    used_pkg = Vset.empty;
    used_vars = Vset.empty;
    local_vars = Smap.empty;
    pkg_pos = Smap.empty;
    func_pos = Smap.empty;
    struct_pos = Smap.empty;
    return_type = [];
    is_return = false }

let clean info = { info with local_vars = Smap.empty }
let merge i si = { i with
                   is_return = i.is_return || si.is_return;
                   used_pkg = Vset.union i.used_pkg si.used_pkg;
                   used_vars =
                     Vset.fold (fun v u -> if Smap.mem v si.local_vars
                                          then u else Vset.add v u)
                       si.used_vars i.used_vars }
let used_pkg info v = { info with used_pkg = Vset.add v info.used_pkg }
let used info v = { info with used_vars = Vset.add v info.used_vars }
let decl info v pos = { info with local_vars = Smap.add v pos info.local_vars }
let ret_info info = { info with is_return = true }

(** Map associating an package name with its associated environnement **)
let all_packages : env Smap.t ref = ref Smap.empty

(** This should be dropped after typing *)
let all_info_packages : env_info Smap.t ref = ref Smap.empty
