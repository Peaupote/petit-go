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
    vars  : typ Smap.t;
    packages : env Smap.t }

let empty_env = {
    types = Smap.empty;
    funcs = Smap.empty;
    vars = Smap.empty;
    packages = Smap.empty }

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

and type_tuple env l = function
  | [] -> assert false
  | x :: [] ->
     let t, te = type_expr env x in
     l && t.left, t.t :: [], te :: []
  | x :: xs ->
     let t, te = type_expr env x in
     let l, tnext, te_next = type_tuple env l xs in
     l && t.left, t.t :: tnext, te :: te_next

and resolve_attr_type env id te e = function
  | Tstruct (s, f) ->
     begin
       try ltyp (Smap.find id.v f), Tattr(te, id.v)
       with Not_found -> compile_error id.position
                          (sprintf "struct `%s` has no field `%s`" s id.v)
     end
  | Tref r -> let t, te = resolve_attr_type env id te e r in
             ltyp t.t, Tunop(Ref, te)
  | t -> compile_error e.position
          (asprintf "this has type `%a` but a struct was expected"
             pp_typ t)

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
     let left, ts, tes = type_tuple env true es in
     { t = Ttuple ts; left = left }, Tetuple tes
  | Ebinop (op, e1, e2) -> type_binop env op e1 e2
  | Eunop (op, e) -> type_unop env op e
  | Eattr (e, id) ->
     let t, te = type_expr env e in
     resolve_attr_type env id te e t.t
  | Ecall(Some pkg, f, ps) when pkg.v = "fmt" && f.v = "Print" ->
     typ Tvoid, Tprint (List.map (fun x -> snd (type_expr env x)) ps)
  | Ecall (None, f, x :: []) when f.v = "new" ->
     begin match x.v with
     | Eident "int" -> typ (Tref Tint), Tnew Tint
     | Eident "string" -> typ (Tref Tint), Tnew Tint
     | Eident "bool" -> typ (Tref Tint), Tnew Tint
     | Eident s ->
        begin try let t = Smap.find s env.types in
                  typ (Tref t), Tnew t
              with Not_found -> compile_error x.position
                                 (sprintf "unkonwn type `%s`" s)
        end
     | _ -> compile_error x.position
             "`new` expects a struct of a primitive type as argument" end
  | Ecall (None, f, _) when f.v = "new" ->
     compile_error f.position "`new` expects only one arguments"
  | Ecall (pkg, f, ps) ->
     let func =
       try
         begin match pkg with
         | None -> Smap.find f.v env.funcs
         | Some pkg ->
            let pkg =
              begin try Smap.find pkg.v env.packages
                    with Not_found ->
                      compile_error pkg.position
                        (asprintf "unknown package `%s`" pkg.v)
              end
            in
            Smap.find f.v pkg.funcs
         end
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

and add_vars ty (ids, env) id =
  if Smap.mem id.v env.vars
  then compile_error id.position
         (asprintf "variable `%s` already exists in the environnement" id.v)
  else id.v :: ids, { env with vars = Smap.add id.v ty env.vars }

and add_vars2 (ids, env) ty id =
  if Smap.mem id.v env.vars
  then compile_error id.position
         (asprintf "variable `%s` already exists in the environnement" id.v)
  else id.v :: ids, { env with vars = Smap.add id.v ty env.vars }

and decl_case env ids ty vs =
  let expected_type =
    match ty with
    | Some ty -> Some (of_ty env ty.v)
    | None -> None in
  let t, te = type_expr env vs in
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
             let ids, env = List.fold_left (add_vars expect) ([], env) ids in
             env, Tdecl (ids, Some expect, Some te)
     end
  | None, Ttuple ts ->
     begin try let ids, env = List.fold_left2 add_vars2 ([], env) ts ids in
         env, Tdecl (ids, None, Some te)
     with Invalid_argument _ ->
           compile_error vs.position
             (sprintf "expecting %d values but got %d"
                (List.length ts) (List.length ids))
     end
  | None, t ->
      let ids, env = List.fold_left (add_vars t) ([], env) ids in
      env, Tdecl (ids, Some t, Some te)
  | Some expect, t when t = expect ->
      let ids, env = List.fold_left (add_vars expect) ([], env) ids in
      env, Tdecl (ids, Some t, Some te)
  | Some expect, t -> type_unexpected vs.position t expect

and type_instruction env = function
  | Inop -> env, Tnop
  | Iexpr e -> env, Texpr (snd (type_expr env e))
  | Idecl (ids, Some ty, None) ->
     let ids, env =
       List.fold_left
         (add_vars (of_ty env ty.v))
         ([], env) ids
     in
     env, Tdecl (ids, None, None)
  | Idecl (ids, ty, Some vs) -> decl_case env ids ty vs
  | Idecl (ids, None, None) ->
     compile_error (List.hd ids).position
       "you must precise type or value of those variables"
  | Iasgn (e1, e2) ->
     let t1, te1 = type_expr env e1 in
     let t2, te2 = type_expr env e2 in
     begin match t1, t2 with
     | t1, _ when not t1.left ->
        compile_error e1.position "this is not a left value"
     | t1, t2 when t1.t <> t2.t -> type_unexpected e2.position t2.t t1.t
     | _ -> env, Tasgn (te1, te2) end
  | Iif (cond, i1, i2) ->
     let t, te = type_expr env cond in
     begin match t.t with
     | Tbool ->
        let _, b1 = type_instruction env i1 in
        let _, b2 = type_instruction env i2 in
        env, Tif (te, b1, b2)
     | t -> type_unexpected cond.position t Tbool end
  | Ifor (cond, i) ->
     let t, te = type_expr env cond in
     begin match t.t with
     | Tbool -> env, Tfor (te, snd (type_instruction env i))
     | t -> type_unexpected cond.position t Tbool end
  | Ireturn e -> env, Treturn (snd (type_expr env e))
  | Iblock is ->
     let env, tis = List.fold_left
       (fun (env, tes) i ->
         let env, ti = type_instruction env i in
         env, ti :: tes) (env, []) is
     in
     env, Tblock (List.rev tis)

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
