open Format
open Ast

module Smap = Map.Make(String)

type typ =
  Any
| Tvoid
| Tnil
| Tint
| Tbool
| Tstring
| Ttuple  of typ list
| Tstruct of ident * typ Smap.t
| Tref    of typ

type etype = { t : typ; left : bool }
let ltyp t = { t = t; left = true }
let typ t  = { t = t; left = false }

let rec pp_product fmt = function
  | [] -> ()
  | x :: [] -> fprintf fmt "%a" pp_typ x
  | x :: xs -> fprintf fmt "%a * " pp_typ x; pp_product fmt xs

and pp_typ fmt = function
  | Any -> fprintf fmt "any type"
  | Tvoid -> fprintf fmt "void"
  | Tnil -> fprintf fmt "nil"
  | Tint -> fprintf fmt "int"
  | Tbool -> fprintf fmt "bool"
  | Tstring -> fprintf fmt "string"
  | Ttuple tps -> fprintf fmt "%a" pp_product tps
  | Tstruct (s, _) -> fprintf fmt "%s" s
  | Tref t -> fprintf fmt "*%a" pp_typ t

type tfunc = typ list * typ list

type env = {
    types : typ Smap.t;
    funcs : tfunc Smap.t;
    vars  : typ Smap.t }

let empty_env = {
    types = Smap.empty;
    funcs = Smap.empty ;
    vars = Smap.empty }

type texpr =
  Tnil
| Teint    of int64
| Testring of string
| Tebool   of bool
| Tident   of ident
| Tetuple  of texpr list
| Tattr    of texpr * ident
| Tcall    of (ident option) * ident * texpr list
| Tunop    of unop * texpr
| Tbinop   of binop * texpr * texpr

type tinstruction =
  Tnop
| Texpr   of texpr
| Tside   of texpr * side
| Tasgn   of texpr * texpr
| Tblock  of tinstruction list
| Tdecl   of ident list * ty option * texpr option
| Treturn of texpr
| Tfor    of texpr * tinstruction
| Tif     of texpr * tinstruction * tinstruction

exception Typing_error of position * string
let typing_error pos msg = raise (Typing_error (pos, msg))
let unknown_type ty =
  typing_error ty.position (sprintf "unknown type %s" ty.v)

let type_unexpected pos t expect =
  let msg =
    asprintf
      ("This expression has type `%a` but an expression " ^^
         "of type `%a` was expected")
      pp_typ t
      pp_typ expect
  in
  typing_error pos msg

let rec of_ty env = function
  | Ast.Tref t -> Tref (of_ty env t.v)
  | Ast.Tstruct { v = "int" ; _ } -> Tint
  | Ast.Tstruct { v = "bool" ; _ } -> Tbool
  | Ast.Tstruct { v = "string" ; _ } -> Tstring
  | Ast.Tstruct s ->
     try Smap.find s.v env.types
     with Not_found -> unknown_type s

let rec type_binop env op e1 e2 =
  let t1, te1 = type_expr env e1 in
  let t2, te2 = type_expr env e2 in
  let t1, t2 = t1.t, t2.t in
  match op with
  | Add | Sub | Div | Mul | Mod ->
     begin match t1, t2 with
     | Tint, Tint -> typ Tint, Tbinop (op, te1, te2)
     | _, Tint -> type_unexpected e1.position t1 Tint
     | _, _ -> type_unexpected e2.position t2 Tint end
  | Lt | Leq | Gt | Geq ->
     begin match t1, t2 with
     | Tint, Tint -> typ Tbool, Tbinop (op, te1, te2)
     | _, Tint -> type_unexpected e1.position t1 Tint
     | _, _ -> type_unexpected e2.position t2 Tint end
  | And | Or ->
     begin match t1, t2 with
     | Tbool, Tbool -> typ Tbool, Tbinop (op, te1, te2)
     | _, Tbool -> type_unexpected e1.position t1 Tbool
     | _, _ -> type_unexpected e2.position t2 Tbool end
  | Eq | Neq ->
     if t1 <> t2 then type_unexpected e2.position t1 t2
     else typ Tbool, Tbinop (op, te1, te2)

and type_unop env op e =
  let t, te = type_expr env e in
  match op with
  | Not -> if t.t <> Tbool then type_unexpected e.position t.t Tbool
          else typ Tbool, Tunop (op, te)
  | Ref -> if not t.left
          then compile_error e.position
                 "invalid argument for &: has to be a left value"
          else typ (Tref t.t), Tunop (op, te)
  | Deref -> if t.t = Tnil
            then compile_error e.position
                   "invalid argument for *: can't be nil"
            else if not t.left
            then compile_error e.position
                   "invalid argument for *: has to be a left value"
            else match t.t with
                 | Tref t -> typ t, Tunop (op, te)
                 | _ -> type_unexpected e.position t.t (Tref Any)


and type_tuple env = function
  | [] -> assert false
  | x :: [] ->
     let t, te = type_expr env x in
     t.t :: [], te :: []
  | x :: xs ->
     let t, te = type_expr env x in
     let tnext, te_next = type_tuple env xs in
     t.t :: tnext, te :: te_next

and type_expr env el =
  match el.v with
  | Enil -> typ Tnil, Tnil
  | Eint i -> typ Tint, Teint i
  | Ebool b -> typ Tbool, Tebool b
  | Estring s -> typ Tstring, Testring s
  | Eident id ->
     begin
       try let t = Smap.find id env.vars in
           ltyp t, Tident id
       with Not_found -> compile_error el.position
                          (sprintf "unkown variable `%s`" id)
     end
  | Etuple es ->
     let ts, tes = type_tuple env es in
     typ (Ttuple ts), Tetuple tes
  | Ebinop (op, e1, e2) -> type_binop env op e1 e2
  | Eunop (op, e) -> type_unop env op e
  | Eattr (e, id) ->
     let t, te = type_expr env e in
     begin match t.t with
     | Tstruct (s, f) ->
        begin
          try typ (Smap.find id.v f), Tattr(te, id.v)
          with Not_found -> compile_error id.position
                             (sprintf "struct `%s` has no field `%s`" s id.v)
        end
     | t -> compile_error e.position
             (asprintf "this has type `%a` but a struct was expected"
                pp_typ t)
     end
  | Ecall (None, f, ps) ->
     let func =
       try Smap.find f.v env.funcs
       with Not_found ->
         compile_error f.position
           (asprintf "unkown function `%s`" f.v)
     in

     let tps =
       try List.map2
             (fun ty p ->
               let tp, tpe = type_expr env p in
               if ty <> tp.t
               then type_unexpected p.position tp.t ty
               else tpe) (fst func) ps
       with Invalid_argument _ ->
         compile_error f.position
           (asprintf "function `%s` expects %d arguments but you gave %d"
              f.v (List.length (fst func)) (List.length ps))
     in

     begin match snd func with
     | [] -> typ Tvoid, Tcall(None, f.v, tps)
     | x :: [] -> typ x, Tcall(None, f.v, tps)
     | xs -> typ (Ttuple xs), Tcall(None, f.v, tps)
     end
  | _ -> assert false

and type_instruction env = function
  | Inop -> Tnop
  | Iexpr e -> Texpr (snd (type_expr env e))
  | Iside (e, s) ->
     let t, te = type_expr env e in
     begin match t.t with
     | Tint -> Tside (te, s)
     | t -> type_unexpected e.position t Tint end
  | Iasgn (e1, e2) ->
     let t1, te1 = type_expr env e1 in
     let t2, te2 = type_expr env e2 in
     begin match t1, t2 with
     | t1, _ when not t1.left ->
        compile_error e1.position "this is not a left value"
     | t1, t2 when t1.t <> t2.t -> type_unexpected e2.position t2.t t1.t
     | _ -> Tasgn (te1, te2) end
  | Iif (cond, i1, i2) ->
     let t, te = type_expr env cond in
     begin match t.t with
     | Tbool ->
        let b1 = type_instruction env i1 in
        let b2 = type_instruction env i2 in
        Tif (te, b1, b2)
     | t -> type_unexpected cond.position t Tbool end
  | Ifor (cond, i) ->
     let t, te = type_expr env cond in
     begin match t.t with
     | Tbool -> Tfor (te, type_instruction env i)
     | t -> type_unexpected cond.position t Tbool end
  | Ireturn e -> Treturn (snd (type_expr env e))
  | Iblock es -> Tblock (List.map (type_instruction env) es)
  | _ -> assert false

let rec check_function_params env = function
  | [] -> []
  | (_, x) :: xs when List.exists (fun (_, y) -> y.v = x.v) xs ->
     (* TODO : better time complexity ? *)
     compile_error x.position
       (sprintf "variable name `%s` is used more than once" x.v)
  | (ty, _) :: xs ->
     (of_ty env ty.v) :: (check_function_params env xs)

let rec check_function_return env = function
  | [] -> []
  | t :: ts -> (of_ty env t.v) :: check_function_return env ts

let type_structure env s =
  let rec check_fields fs = function
    | [] -> fs
    | (_, x) :: xs when List.exists (fun (_, y) -> y.v = x.v) xs ->
       compile_error x.position
         (sprintf "field name `%s` is used more than once" x.v)
    | ({ v = Ast.Tstruct name; _ }, _) :: _ when name.v = s.v.s_name.v ->
       compile_error name.position
         (sprintf "infinite sized-stack type `%s`" name.v)
    | (ty, x) :: xs -> check_fields (Smap.add x.v (of_ty env ty.v) fs) xs
  in
  let fields = check_fields Smap.empty s.v.s_body in
  { env with types = Smap.add s.v.s_name.v
                       (Tstruct (s.v.s_name.v, fields)) env.types }

let type_function env f =
  let vars =
    List.fold_left (fun vars (ty, id) -> Smap.add id.v (of_ty env ty.v) vars)
      Smap.empty f.v.f_params
  in
  let _ = type_instruction { env with vars = vars } f.v.f_body in
  env

let type_prog env prog =
  (* Add structures without content *)
  let env =
    List.fold_left
      (fun env s ->
        if Smap.mem s.v.s_name.v env.types
        then compile_error s.v.s_name.position
               (sprintf "structure `%s` already exists" s.v.s_name.v);

        { env with
          types = Smap.add s.v.s_name.v
                    (Tstruct (s.v.s_name.v, Smap.empty)) env.types })
      env prog.p_structures
  in

  (* Check and add functions *)
  let env =
    List.fold_left
      (fun env f ->
        if Smap.mem f.v.f_name.v env.funcs
        then compile_error f.v.f_name.position
               (sprintf "function `%s` already exists" f.v.f_name.v);

        let ps = check_function_params env f.v.f_params in
        let ret = check_function_return env f.v.f_return in
        { env with
          funcs = Smap.add f.v.f_name.v (ps, ret) env.funcs })
      env prog.p_functions in

  (* Add structure content *)
  let env = List.fold_left type_structure env prog.p_structures in

  (* Type functions *)
  let env = List.fold_left type_function env prog.p_functions in

  env
