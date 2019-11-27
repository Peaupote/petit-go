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

(* TODO : keep track of where things were declared *)
type env = {
    types : typ Smap.t;
    funcs : tfunc Smap.t;
    vars  : typ Smap.t;
    packages : env Smap.t }

let empty_env =
  { types = Smap.empty;
    funcs = Smap.empty;
    vars = Smap.empty;
    packages = Smap.empty }

module Vset = Set.Make(String)

type env_info = {
    used_pkg   : Vset.t;
    used_vars  : Vset.t;
    local_vars : Vset.t; }

let empty_info = {
    used_pkg = Vset.empty;
    used_vars = Vset.empty;
    local_vars = Vset.empty;
  }

let clean info = { info with local_vars = Vset.empty }
let merge i si = { i with used_pkg = Vset.union i.used_pkg si.used_pkg;
                          used_vars =
                            Vset.fold (fun v u -> if Vset.mem v si.local_vars
                                                 then u else Vset.add v u)
                              si.used_vars i.used_vars }
let used_pkg info v = { info with used_pkg = Vset.add v info.used_pkg }
let used info v = { info with used_vars = Vset.add v info.used_vars }
let decl info v = { info with local_vars = Vset.add v info.local_vars }
let diff s v = Smap.find_first (fun x -> not (Vset.mem x v)) s

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

let rec type_binop info env op e1 e2 =
  let info, t1, te1 = type_expr info env e1 in
  let info, t2, te2 = type_expr info env e2 in
  let t1, t2 = t1.t, t2.t in
  match op with
  | Add | Sub | Div | Mul | Mod ->
     begin match t1, t2 with
     | Tint, Tint -> info, typ Tint, Tbinop (op, te1, te2)
     | _, Tint -> type_unexpected e1.position t1 Tint
     | _, _ -> type_unexpected e2.position t2 Tint end
  | Lt | Leq | Gt | Geq ->
     begin match t1, t2 with
     | Tint, Tint -> info, typ Tbool, Tbinop (op, te1, te2)
     | _, Tint -> type_unexpected e1.position t1 Tint
     | _, _ -> type_unexpected e2.position t2 Tint end
  | And | Or ->
     begin match t1, t2 with
     | Tbool, Tbool -> info, typ Tbool, Tbinop (op, te1, te2)
     | _, Tbool -> type_unexpected e1.position t1 Tbool
     | _, _ -> type_unexpected e2.position t2 Tbool end
  | Eq | Neq ->
     if t1 <> t2 then type_unexpected e2.position t1 t2
     else info, typ Tbool, Tbinop (op, te1, te2)

and type_unop info env op e =
  let info, t, te = type_expr info env e in
  match op with
  | Not -> if t.t <> Tbool then type_unexpected e.position t.t Tbool
          else info, typ Tbool, Tunop (op, te)
  | Ref -> if not t.left
          then compile_error e.position
                 "invalid argument for &: has to be a left value"
          else info, typ (Tref t.t), Tunop (op, te)
  | Deref -> if t.t = Tnil
            then compile_error e.position
                   "invalid argument for *: can't be nil"
            else if not t.left
            then compile_error e.position
                   "invalid argument for *: has to be a left value"
            else match t.t with
                 | Tref t -> info, typ t, Tunop (op, te)
                 | _ -> type_unexpected e.position t.t (Tref Any)

and type_tuple info env l = function
  | [] -> assert false
  | x :: [] ->
     let info, t, te = type_expr info env x in
     info, l && t.left, t.t :: [], te :: []
  | x :: xs ->
     let info, t, te = type_expr info env x in
     let info, l, tnext, te_next = type_tuple info env l xs in
     info, l && t.left, t.t :: tnext, te :: te_next

and resolve_attr_type info env id te e = function
  | Tstruct (s, f) ->
     begin
       try info, ltyp (Smap.find id.v f), Tattr(te, id.v)
       with Not_found -> compile_error id.position
                          (sprintf "struct `%s` has no field `%s`" s id.v)
     end
  | Tref r -> let info, t, te = resolve_attr_type info env id te e r in
             info, ltyp t.t, Tunop(Ref, te)
  | t -> compile_error e.position
          (asprintf "this has type `%a` but a struct was expected"
             pp_typ t)

and type_expr info env el =
  match el.v with
  (* primitives types *)
  | Enil -> info, typ Tnil, Tnil
  | Eint i -> info, typ Tint, Teint i
  | Ebool b -> info, typ Tbool, Tebool b
  | Estring s -> info, typ Tstring, Testring s
  | Eident "_" -> info, typ Tvoid, Tident "_"
  | Eident id ->
     begin
       try let t = Smap.find id env.vars in
           used info id, ltyp t, Tident id
       with Not_found -> compile_error el.position
                          (sprintf "unkown variable `%s`" id)
     end
  | Etuple es ->
     let info, left, ts, tes = type_tuple info env true es in
     info, { t = Ttuple ts; left = left }, Tetuple tes

  (* primitives operations *)
  | Ebinop (op, e1, e2) -> type_binop info env op e1 e2
  | Eunop (op, e) -> type_unop info env op e
  | Eattr (e, id) ->
     let info, t, te = type_expr info env e in
     resolve_attr_type info env id te e t.t

  (* primitives function calls *)
  | Ecall(Some pkg, f, ps) when pkg.v = "fmt" && f.v = "Print" ->
     (* TODO : dont lookup each time *)
     if not (Smap.mem "fmt" env.packages)
     then compile_error pkg.position "unkown package fmt";
     let info, ps =
       List.fold_right
         (fun p (info, ps) ->
           let info, _, te = type_expr info env p in
           info, te :: ps)
         ps (info, []) in
     used_pkg info "fmt", typ Tvoid, Tprint ps
  | Ecall (None, f, x :: []) when f.v = "new" ->
     begin match x.v with
     | Eident "int" -> info, typ (Tref Tint), Tnew Tint
     | Eident "string" -> info, typ (Tref Tint), Tnew Tint
     | Eident "bool" -> info, typ (Tref Tint), Tnew Tint
     | Eident s ->
        begin try let t = Smap.find s env.types in
                  info, typ (Tref t), Tnew t
              with Not_found -> compile_error x.position
                                 (sprintf "unkonwn type `%s`" s)
        end
     | _ -> compile_error x.position
             "`new` expects a struct of a primitive type as argument" end
  | Ecall (None, f, _) when f.v = "new" ->
     compile_error f.position "`new` expects only one arguments"

  (* general function call *)
  | Ecall (pkg, f, ps) ->
     let info, func =
       try
         begin match pkg with
         | None -> info, Smap.find f.v env.funcs
         | Some pkg_name ->
            let pkg =
              begin try Smap.find pkg_name.v env.packages
                    with Not_found ->
                      compile_error pkg_name.position
                        (asprintf "unknown package `%s`" pkg_name.v)
              end
            in
            used_pkg info pkg_name.v, Smap.find f.v pkg.funcs
         end
       with Not_found ->
         compile_error f.position
           (asprintf "unkown function `%s`" f.v)
     in

     let info, tps =
       try List.fold_right2
             (fun ty p (info, tps) ->
               let info, tp, tpe = type_expr info env p in
               if ty <> tp.t
               then type_unexpected p.position tp.t ty
               else info, tpe :: tps) (fst func) ps (info, [])
       with Invalid_argument _ ->
         compile_error f.position
           (asprintf "function `%s` expects %d arguments but you gave %d"
              f.v (List.length (fst func)) (List.length ps))
     in

     begin match snd func with
     | [] -> info, typ Tvoid, Tcall(None, f.v, tps)
     | x :: [] -> info, typ x, Tcall(None, f.v, tps)
     | xs -> info, typ (Ttuple xs), Tcall(None, f.v, tps)
     end

and add_vars ty (info, ids, env) id =
  if id.v <> "_" && Vset.mem id.v info.local_vars
  then compile_error id.position
         (asprintf "variable `%s` has already been declared in this block" id.v)
  else { info with local_vars = Vset.add id.v info.local_vars },
       id.v :: ids,
       { env with vars = Smap.add id.v ty env.vars }

and add_vars2 (info, ids, env) ty id =
  if id.v <> "_" && Vset.mem id.v info.local_vars
  then compile_error id.position
         (asprintf "variable `%s` has already been declared in this block" id.v)
  else { info with local_vars = Vset.add id.v info.local_vars },
       id.v :: ids,
       { env with vars = Smap.add id.v ty env.vars }

and decl_case info env ids ty vs =
  let expected_type =
    match ty with
    | Some ty -> Some (of_ty env ty.v)
    | None -> None in
  let info, t, te = type_expr info env vs in
  match expected_type, t.t with
  | Some _, Ttuple ts when List.length ts <> List.length ids ->
     compile_error vs.position
       (sprintf "expecting %d values but got %d"
          (List.length ts) (List.length ids))
  | Some expect, Ttuple ts ->
     begin try let t = List.find ((<>) expect) ts in
               (* TODO : more precise location  *)
               type_unexpected vs.position t expect
           with Not_found ->
             let info, ids, env =
               List.fold_left (add_vars expect) (info, [], env) ids in
             info, env, Tdecl (ids, Some expect, Some te)
     end
  | None, Ttuple ts ->
     begin try let info, ids, env =
                 List.fold_left2 add_vars2 (info, [], env) ts ids in
         info, env, Tdecl (ids, None, Some te)
     with Invalid_argument _ ->
           compile_error vs.position
             (sprintf "expecting %d values but got %d"
                (List.length ts) (List.length ids))
     end
  | None, t ->
     let info, ids, env =
       List.fold_left (add_vars t) (info, [], env) ids in
     info, env, Tdecl (ids, Some t, Some te)
  | Some expect, t when t = expect ->
     let info, ids, env =
       List.fold_left (add_vars expect) (info, [], env) ids in
     info, env, Tdecl (ids, Some t, Some te)
  | Some expect, t -> type_unexpected vs.position t expect

and check_return_no_underscore te =
  match te with
  | Tetuple ts -> List.exists check_return_no_underscore ts
  | Tident "_" -> true
  | _ -> false

and type_instruction info env = function
  | Inop -> info, env, Tnop
  | Iexpr e ->
     let info, _, te = type_expr info env e in
     info, env, Texpr te
  | Idecl (ids, Some ty, None) ->
     let info, ids, env =
       List.fold_left
         (add_vars (of_ty env ty.v))
         (info, [], env) ids
     in
     info, env, Tdecl (ids, None, None)
  | Idecl (ids, ty, Some vs) -> decl_case info env ids ty vs
  | Idecl (ids, None, None) ->
     compile_error (List.hd ids).position
       "you must precise type or value of those variables"
  | Iasgn (e1, e2) ->
     let info, t1, te1 = type_expr info env e1 in
     let info, t2, te2 = type_expr info env e2 in
     begin match t1, t2 with
     | t1, _ when not t1.left ->
        compile_error e1.position "this is not a left value"
     | t1, t2 when t1.t <> t2.t -> type_unexpected e2.position t2.t t1.t
     | _ -> info, env, Tasgn (te1, te2) end
  | Iif (cond, i1, i2) ->
     let info, t, te = type_expr info env cond in
     begin match t.t with
     | Tbool ->
        let info, _, b1 = type_instruction info env i1 in
        let info, _, b2 = type_instruction info env i2 in
        info, env, Tif (te, b1, b2)
     | t -> type_unexpected cond.position t Tbool end
  | Ifor (cond, i) ->
     let info, t, te = type_expr info env cond in
     begin match t.t with
     | Tbool ->
        let info, _, b = type_instruction info env i in
        info, env, Tfor (te, b)
     | t -> type_unexpected cond.position t Tbool end
  | Ireturn e ->
     let info, _, te = type_expr info env e in
     if check_return_no_underscore te
     then compile_error e.position "you can't return `_`";
     info, env, Treturn te
  | Iblock is ->
     let sinfo, env, tis = List.fold_left
       (fun (sinfo, env, tes) i ->
         let sinfo, env, ti = type_instruction sinfo env i in
         sinfo, env, ti :: tes) (clean info, env, []) is
     in
     merge info sinfo, env, Tblock (List.rev tis)

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

let type_function (info, env) f =
  let finfo, vars =
    List.fold_left (fun (finfo, vars) (ty, id) ->
        decl finfo id.v, Smap.add id.v (of_ty env ty.v) vars)
      (info, Smap.empty) f.v.f_params
  in
  let finfo, _, _ = type_instruction finfo { env with vars = vars } f.v.f_body in
  { info with used_pkg = Vset.union finfo.used_pkg info.used_pkg }, env

let type_prog env prog =
  let env =
    List.fold_left
      (fun env pkg ->
        if pkg.v = "fmt"
        then { env with packages = Smap.add "fmt" empty_env env.packages }
        else compile_error pkg.position (sprintf "unkonw package `%s`" pkg.v))
      env prog.p_imports in

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
  let info, env = List.fold_left type_function
                    (empty_info, env) prog.p_functions in

  (* Look for unused packages *)
  Smap.iter (fun p _ -> if not (Vset.mem p info.used_pkg)
                       then error (sprintf "unused package `%s`" p))
    env.packages;

  env
