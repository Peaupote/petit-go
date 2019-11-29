open Format
open Config
open Ast
open Graph
open Error

type typ =
  Tvoid
| Tnil
| Tint
| Tbool
| Tstring
| Ttuple  of typ list
| Tstruct of ident
| Tref    of typ

type etype = { t : typ; left : bool }
let ltyp t = { t = t; left = true }
let typ t  = { t = t; left = false }

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

let typ_eq t1 t2 =
  match t1, t2 with
  | Tnil, Tref _ | Tref _, Tnil -> true
  | _ -> t1 = t2

let typ_neq t1 t2 = not (typ_eq t1 t2)

type tfunc = typ list * typ list
type tstruct = typ Smap.t

type env = {
    structs : tstruct Smap.t;
    types : typ Smap.t;
    funcs : tfunc Smap.t;
    vars  : typ Smap.t;
    packages : env Smap.t }

let empty_env =
  { structs = Smap.empty;
    types = Smap.empty;
    funcs = Smap.empty;
    vars = Smap.empty;
    packages = Smap.empty }

let add_env v t env = { env with vars = Smap.add v t env.vars }

type env_info = {
    used_pkg    : Vset.t;
    used_vars   : Vset.t;
    local_vars  : position Smap.t;
    pkg_pos     : position Smap.t;
    func_pos    : position Smap.t;
    struct_pos  : position Smap.t;
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

let type_unexpected pos t expect =
  let msg =
    asprintf
      ("this expression has type `%a` but an expression " ^^
         "of type `%a` was expected")
      pp_typ t
      pp_typ expect
  in
  compile_error pos msg

let return_type_unexpected pos t e =
  compile_error pos
    (asprintf "this function returns `%a` but you return a `%a`"
       pp_typ e pp_typ t)

let rec of_ty env = function
  | Ast.Tref t -> Tref (of_ty env t.v)
  | Ast.Tstruct { v = "int" ; _ } -> Tint
  | Ast.Tstruct { v = "bool" ; _ } -> Tbool
  | Ast.Tstruct { v = "string" ; _ } -> Tstring
  | Ast.Tstruct s ->
     try Smap.find s.v env.types
     with Not_found -> unknown_type s.position s.v

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
     if te1 = Tenil && te2 = Tenil
     then nil_cmp_error e1.position;
     if typ_neq t1 t2 then type_unexpected e2.position t1 t2
     else info, typ Tbool, Tbinop (op, te1, te2)

and type_unop info env op e =
  let info, t, te = type_expr info env e in
  match op with
  | Not -> if t.t <> Tbool then type_unexpected e.position t.t Tbool
          else info, typ Tbool, Tunop (op, te)
  | Ref -> if not t.left
          then invalid_argument e.position "&" "has to be a left value"
          else info, typ (Tref t.t), Tunop (op, te)
  | Deref -> if t.t = Tnil
            then error_left_value e.position "*"
            else if not t.left
            then error_left_value e.position "*"
            else match t.t with
                 | Tref t -> info, ltyp t, Tunop (op, te)
                 | _ -> invalid_argument e.position "*"
                         (asprintf "type %a is not a reference" pp_typ t.t)

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
  | Tstruct s ->
     begin
       try let fields = Smap.find s env.structs in
           info, ltyp (Smap.find id.v fields), Tattr(te, id.v)
       with Not_found -> unknown_field id.position s id.v
     end
  | Tref r -> let info, t, te = resolve_attr_type info env id te e r in
             info, ltyp t.t, Tunop(Ref, te)
  | t -> compile_error e.position
          (asprintf "this has type `%a` but a struct was expected"
             pp_typ t)

and find_func info env pkg f =
  try
    begin match pkg with
    | None -> info, Smap.find f.v env.funcs
    | Some pkg_name ->
       let pkg =
         begin try Smap.find pkg_name.v env.packages
               with Not_found -> unknown_pkg pkg_name.position pkg_name.v
         end
       in
       used_pkg info pkg_name.v, Smap.find f.v pkg.funcs
    end
  with Not_found -> unknown_func f.position f.v

and rm = function
  | Some x -> Some x.v
  | None -> None

and ret = function
  | [] -> Tvoid
  | x :: [] -> x
  | xs -> Ttuple xs

and type_expr info env el =
  match el.v with
  (* primitives types *)
  | Enil -> info, typ Tnil, Tenil
  | Eint i -> info, typ Tint, Teint i
  | Ebool b -> info, typ Tbool, Tebool b
  | Estring s -> info, typ Tstring, Testring s
  | Eident "_" -> info, typ Tvoid, Tident "_"
  | Eident id ->
     begin
       try let t = Smap.find id env.vars in
           used info id, ltyp t, Tident id
       with Not_found -> unknown_var el.position id
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
     then unknown_pkg pkg.position "fmt";
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
     | Eident "string" -> info, typ (Tref Tstring), Tnew Tint
     | Eident "bool" -> info, typ (Tref Tbool), Tnew Tint
     | Eident s ->
        begin try let t = Smap.find s env.types in
                  info, typ (Tref t), Tnew t
              with Not_found -> unknown_type x.position s
        end
     | _ -> compile_error x.position
             "`new` expects a struct or a primitive type as argument" end
  | Ecall (None, f, _) when f.v = "new" ->
     compile_error f.position "`new` expects only one arguments"

  (* general function call *)
  | Ecall (pkg, f, ({ v = Ecall(_, _, _); _ } as c) :: []) ->
     let info, t, te = type_expr info env c in
     let info, (ps_t, ret_t) = find_func info env pkg f in
     begin match t.t, ps_t with
     | t, t' :: [] when typ_eq t t' -> info, typ (ret ret_t),
                                 Tcall (rm pkg, f.v, te::[])
     | Ttuple ts, ts' when List.for_all2 typ_eq ts ts' ->
        info, typ (ret ret_t), Tcall (rm pkg, f.v, te::[])
     | t, t' -> type_unexpected c.position t (ret t')
     end
  | Ecall (pkg, f, ps) ->
     let info, (ps_t, ret_t) = find_func info env pkg f in
     let info, tps =
       try List.fold_right2
             (fun ty p (info, tps) ->
               let info, tp, tpe = type_expr info env p in
               if typ_neq ty tp.t
               then type_unexpected p.position tp.t ty
               else info, tpe :: tps) ps_t ps (info, [])
       with Invalid_argument _ ->
         let func_pos = Smap.find f.v info.func_pos in
         args_nb_error func_pos f.position f.v
           (List.length ps_t) (List.length ps)
     in
     info, typ (ret ret_t), Tcall(rm pkg, f.v, tps)

and add_vars ty (info, ids, env) id =
  if id.v = "_"
  then info, id.v :: ids, env
  else try let pos = Smap.find id.v info.local_vars in
           already_declared pos id.position id.v
       with Not_found ->
         decl info id.v id.position,
         id.v :: ids,
         add_env id.v ty env

and add_vars2 (info, ids, env) ty id =
  if id.v = "_"
  then info, id.v :: ids, env
  else try let pos = Smap.find id.v info.local_vars in
           already_declared pos id.position id.v
       with Not_found ->
         decl info id.v id.position,
         id.v :: ids,
         add_env id.v ty env

and is_nil = function
  | Tenil -> true
  | Tetuple ts -> List.exists ((=) Tenil) ts
  | _ -> false

and decl_case info env ids ty vs =
  let expected_type =
    match ty with
    | Some ty -> Some (of_ty env ty.v)
    | None -> None in
  let info, t, te = type_expr info env vs in
  match expected_type, t.t with
  | Some _, Ttuple ts when List.length ts <> List.length ids ->
     decl_nb_error vs.position (List.length ts) (List.length ids)
  | Some expect, Ttuple ts ->
     begin try let t = List.find (typ_neq expect) ts in
               (* TODO : more precise location  *)
               type_unexpected vs.position t expect
           with Not_found ->
             let info, ids, env =
               List.fold_left (add_vars expect) (info, [], env) ids in
             info, env, Tdecl (ids, Some expect, Some te)
     end
  | None, _ when is_nil te -> untyped_nil vs.position
  | None, Ttuple ts ->
     begin try let info, ids, env =
                 List.fold_left2 add_vars2 (info, [], env) ts ids in
         info, env, Tdecl (ids, None, Some te)
     with Invalid_argument _ ->
       decl_nb_error vs.position (List.length ts) (List.length ids)
     end
  | None, t ->
     let info, ids, env =
       List.fold_left (add_vars t) (info, [], env) ids in
     info, env, Tdecl (ids, Some t, Some te)
  | Some expect, t when typ_eq t expect ->
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
  | Idecl (ids, None, None) -> untyped_decl (List.hd ids).position
  | Iasgn (e1, e2) ->
     let info, t1, te1 = type_expr info env e1 in
     let info, t2, te2 = type_expr info env e2 in
     begin match t1, t2 with
     | t1, _ when not t1.left ->
        asgn_not_left_value e1.position
     | t1, t2 when typ_neq t1.t t2.t ->
        type_unexpected e2.position t2.t t1.t
     | _ -> info, env, Tasgn (te1, te2) end
  | Iif (cond, i1, i2) ->
     let info, t, te = type_expr info env cond in
     begin match t.t with
     | Tbool ->
        let info1, _, b1 = type_instruction info env i1 in
        let info, _, b2 = type_instruction { info1 with is_return = false } env i2 in
        { info with is_return = info1.is_return && info.is_return }, env, Tif (te, b1, b2)
     | t -> type_unexpected cond.position t Tbool end
  | Ifor (cond, i) ->
     let info, t, te = type_expr info env cond in
     begin match t.t with
     | Tbool ->
        let info, _, b = type_instruction info env i in
        { info with is_return = false }, env, Tfor (te, b)
     | t -> type_unexpected cond.position t Tbool end
  | Ireturn e ->
     let info, t, te = type_expr info env e in
     if check_return_no_underscore te
     then return_drop e.position;
     begin match info.return_type with
     | [] -> if t.t <> Tnil (* BUG HERE *)
            then return_type_unexpected e.position t.t Tvoid
            else ret_info info, env, Treturn te
     | rt :: [] when typ_eq rt t.t -> ret_info info, env, Treturn te
     | rt when typ_eq t.t (Ttuple rt) -> ret_info info, env, Treturn te
     | rt -> return_type_unexpected e.position t.t (Ttuple rt)
     end
  | Iblock is ->
     let sinfo, env, tis = List.fold_left
       (fun (sinfo, env, tes) i ->
         let sinfo, env, ti = type_instruction sinfo env i in
         sinfo, env, ti :: tes) (clean info, env, []) is
     in
     try let (v, pos) =
           Smap.find_first
             (fun x -> x <> "_" && not (Vset.mem x sinfo.used_vars))
             sinfo.local_vars
         in
         unused_var pos v
     with Not_found -> merge info sinfo, env, Tblock (List.rev tis)

let rec check_function_params env = function
  | [] -> []
  | (ty, x) :: xs ->
     match List.find_opt (fun (_, y) -> y.v = x.v) xs with
     | Some (_, y) -> redondant_param_name y.position x.position x.v
     | None -> (of_ty env ty.v) :: (check_function_params env xs)


let rec check_function_return env = function
  | [] -> []
  | t :: ts -> (of_ty env t.v) :: check_function_return env ts

let type_structure env s =
  let rec check_fields fs = function
    | [] -> fs
    | (ty, x) :: xs ->
       match List.find_opt (fun (_, y) -> y.v = x.v) xs with
       | Some (_, y) -> redondant_field_name y.position x.position x.v
       | None -> check_fields (Smap.add x.v (of_ty env ty.v) fs) xs
  in
  let fields = check_fields Smap.empty s.v.s_body in
  { env with types = Smap.add s.v.s_name.v (Tstruct s.v.s_name.v) env.types;
             structs = Smap.add s.v.s_name.v fields env.structs }

let type_function (info, env) f =
  let (_, ret_type) = Smap.find f.v.f_name.v env.funcs in
  let finfo, vars =
    List.fold_left (fun (finfo, vars) (ty, id) ->
        decl finfo id.v id.position, Smap.add id.v (of_ty env ty.v) vars)
      ({ info with return_type = ret_type }, Smap.empty) f.v.f_params
  in
  let finfo, _, _ = type_instruction finfo { env with vars = vars } f.v.f_body in
  if ret_type <> [] && not finfo.is_return
  then compile_error f.position "this function may returns nothing";
  { info with used_pkg = Vset.union finfo.used_pkg info.used_pkg }, env

let type_prog env prog =
  let info, env =
    List.fold_left
      (fun (info, env) pkg ->
        if pkg.v = "fmt"
        then { info with pkg_pos = Smap.add pkg.v pkg.position info.pkg_pos },
             { env with packages = Smap.add "fmt" empty_env env.packages }
        else unknown_pkg pkg.position pkg.v)
      (empty_info, env) prog.p_imports in

  (* Add structures without content *)
  let (info, env) =
    List.fold_left
      (fun (info, env) s ->
        try let s_pos = Smap.find s.v.s_name.v info.struct_pos in
            struct_already_exists s_pos s.v.s_name.position s.v.s_name.v
        with Not_found ->
          { info with
            struct_pos = Smap.add s.v.s_name.v s.position info.struct_pos },
          { env with
            types = Smap.add s.v.s_name.v (Tstruct s.v.s_name.v) env.types })
      (info, env) (List.rev prog.p_structures)
  in

  (* Check and add functions *)
  let (info, env) =
    List.fold_left
      (fun (info, env) f ->
        try let f_pos = Smap.find f.v.f_name.v info.func_pos in
            func_already_exists f_pos f.v.f_name.position f.v.f_name.v
        with Not_found ->

          let ps = check_function_params env f.v.f_params in
          let ret = check_function_return env f.v.f_return in
          { info with
            func_pos = Smap.add f.v.f_name.v f.position info.func_pos },
          { env with
            funcs = Smap.add f.v.f_name.v (ps, ret) env.funcs })
      (info, env) (List.rev prog.p_functions) in

  (* Add structure content *)
  let env = List.fold_left type_structure env prog.p_structures in

  (* Check cycles in types *)
  let g = List.map (fun s -> vertex s.v.s_name.v) prog.p_structures in
  List.iter (fun s ->
      match Smap.find s.v.s_name.v env.types with
      | Tstruct s ->
         Smap.iter
           (fun _ t ->
             match t with
             | Tstruct s' -> add_node g s' s
             | _ -> ()) (Smap.find s env.structs)
      | _ -> assert false)
    prog.p_structures;

  (* TODO : better error information *)
  let _ = match has_cycle g with
    | Some cycle -> cycle_struct cycle
    | None -> ()
  in

  (* Type functions *)
  let info, env = List.fold_left type_function
                    (info, env) prog.p_functions in

  (* Look for unused packages *)
  Smap.iter (fun p pos -> if not (Vset.mem p info.used_pkg)
                         then unused_pkg pos p)
    info.pkg_pos;

  env
