open Format
open Config
open Ast


(** Type for the typer *)
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

(** All information about the package
    that you want to keep after typing *)
type env = {
    structs : tstruct Smap.t;
    types : typ Smap.t;
    funcs : tfunc Smap.t;
    vars  : typ Smap.t;
    packages : Vset.t }

let empty_env =
  { structs = Smap.empty;
    types = Smap.empty;
    funcs = Smap.empty;
    vars = Smap.empty;
    packages = Vset.empty }

let add_env v t env = { env with vars = Smap.add v t env.vars }

(** Map associating an package name with its associated environnement **)
let all_packages : env Smap.t ref = ref Smap.empty
