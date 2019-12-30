open Format
open Config
open Ast
open Graph
open Error

let cur_pkg = ref ""

(** Error handling *)

let pp_func fmt (params, t_ret) =
  let rec aux fmt = function
    | [] -> ()
    | (p, t) :: [] -> fprintf fmt "%s %a" p pp_typ t
    | (p, t) :: ps -> fprintf fmt "%s %a, " p pp_typ t; aux fmt ps
  in
  fprintf fmt "(%a) %a" aux params pp_typ (Ttuple t_ret)

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

(** Convert a Parser type to a Typer type *)
let rec of_ty env = function
  | Tyref t -> Tref (of_ty env t.v)
  | Tystruct (None, s) when Smap.mem s.v env.types ->
     Smap.find s.v env.types
  | Tystruct (Some pkg, s) ->
     let pkg =
       try Smap.find pkg.v !all_packages
       with Not_found -> unknown_pkg env pkg.position pkg.v in
     begin
       try Smap.find s.v pkg.types
       with Not_found -> unknown_type pkg s.position s.v
     end
  | Tystruct (None, { v = "int" ; _ }) -> Tint
  | Tystruct (None, { v = "bool" ; _ }) -> Tbool
  | Tystruct (None, { v = "string" ; _ }) -> Tstring
  | Tystruct (None, s) -> unknown_type env s.position s.v

(** Typing functions *)

let rec type_binop info env op e1 e2 =
  let info, t1, te1 = type_expr info env e1 in
  let info, t2, te2 = type_expr info env e2 in
  let t1, t2 = t1.t, t2.t in
  match op with
  | Add | Sub | Div | Mul | Mod ->
     begin match t1, t2 with
     | Tint, Tint -> info, typ Tint, Tbinop (op, te1, te2, Tint)
     | _, Tint -> type_unexpected e1.position t1 Tint
     | _, _ -> type_unexpected e2.position t2 Tint end
  | Lt | Leq | Gt | Geq ->
     begin match t1, t2 with
     | Tint, Tint -> info, typ Tbool, Tbinop (op, te1, te2, Tint)
     | _, Tint -> type_unexpected e1.position t1 Tint
     | _, _ -> type_unexpected e2.position t2 Tint end
  | And | Or ->
     begin match t1, t2 with
     | Tbool, Tbool -> info, typ Tbool, Tbinop (op, te1, te2, Tbool)
     | _, Tbool -> type_unexpected e1.position t1 Tbool
     | _, _ -> type_unexpected e2.position t2 Tbool end
  | Eq | Neq ->
     if te1 = Tenil && te2 = Tenil
     then nil_cmp_error e1.position;
     if typ_neq t1 t2 then type_unexpected e2.position t1 t2
     else info, typ Tbool, Tbinop (op, te1, te2, t1)

and type_unop info env op e =
  let info, t, te = type_expr info env e in
  match op with
  | Not -> if t.t <> Tbool then type_unexpected e.position t.t Tbool
          else info, typ Tbool, Tunop (op, te, Tbool)
  | Ref -> if not t.left
          then invalid_argument e.position "&" "has to be a left value"
          else info, typ (Tref t.t), Tunop (op, te, Tref t.t)
  | Deref -> if t.t = Tnil
            then error_left_value e.position "*"
            else if not t.left
            then error_left_value e.position "*"
            else match t.t with
                 | Tref t -> info, ltyp t, Tunop (op, te, t)
                 | _ -> invalid_argument e.position "*"
                         (asprintf "type %a is not a reference" pp_typ t.t)

and type_tuple info env lst =
  let rec aux info l = function
    | [] -> info, false, Tvoid :: [], Tenil :: []
    | x :: [] ->
       let info, t, te = type_expr info env x in
       info, l && t.left, t.t :: [], te :: []
    | x :: xs ->
       let info, t, te = type_expr info env x in
       let info, l, tnext, te_next = aux info l xs in
       info, l && t.left, t.t :: tnext, te :: te_next
  in
  let info, l, ts, tes = aux info true lst in
  match ts with
  | t :: [] -> info, { t = t; left = l }, List.hd tes
  | ts -> info, { t = Ttuple ts; left = l }, Tetuple tes

(** Follow references until found a structure on which
    we want to get attribute id *)
and resolve_attr_type info env id te e t =
  let rec aux = function
    | Tstruct s ->
       begin
         try let fields = Smap.find s env.structs in
             let tau = List.assoc id.v fields in
             info, tau, s, te
         with Not_found -> unknown_field env id.position s id.v
       end
    | (Tref r) as t -> let info, tau, s, te = aux r in
                      info, tau, s, Tunop (Ref, te, t)
    | t -> compile_error e.position
            (asprintf "this has type `%a` but a struct was expected"
               pp_typ t)
  in
  let info, tau, s, te = aux t in
  info, ltyp tau, Tattr(te, s, id.v, tau)

(** Look for the function f in the package pkg
    Mark the package as used if f exists *)
and clear_name (params, t_ret) =
  let rec aux = function
    | [] -> []
    | (_, ptyp) :: ps -> ptyp :: aux ps
  in
  aux params, t_ret

and func_name pkg fname =
  let pre = match pkg with Some pkg_name -> pkg_name.v | None -> !cur_pkg in
  sprintf "%s_%s" pre fname

and find_func info env pkg f =
  try
    begin match pkg with
    | None -> info, clear_name (Smap.find f.v env.funcs)
    | Some pkg_name ->
       let pkg =
         try Smap.find pkg_name.v !all_packages
         with Not_found -> unknown_pkg env pkg_name.position pkg_name.v
       in
       if not (Vset.mem pkg_name.v env.packages)
       then unknown_pkg env pkg_name.position pkg_name.v;
       try used_pkg info pkg_name.v, clear_name (Smap.find f.v pkg.funcs)
       with Not_found -> unknown_func pkg f.position f.v
    end
  with Not_found -> unknown_func env f.position f.v

and pkg_info info = function
  | None -> info
  | Some pkg -> Smap.find pkg.v !all_info_packages

and rm = function
  | Some x -> Some x.v
  | None -> None

and ret = function
  | [] -> Tvoid
  | x :: [] -> x
  | xs -> Ttuple xs

(** Typing of an expression. Returns
    - information about current state of the typing
    - the type of the expression (and if it is a left value)
    - a typed syntax tree *)
and type_expr info env el =
  match el.v with
  (* primitives types *)
  | Enil -> info, typ Tnil, Tenil
  | Eint i -> info, typ Tint, Teint i
  | Ebool b -> info, typ Tbool, Tebool b
  | Estring s -> info, typ Tstring, Testring s
  | Eident "_" -> info, typ Tvoid, Tident ("_", Tnil)
  | Eident id ->
     begin
       try let t = Smap.find id env.vars in
           used info id, ltyp t, Tident (id, t)
       with Not_found -> unknown_var env el.position id
     end
  | Etuple es -> type_tuple info env es

  (* primitives operations *)
  | Ebinop (op, e1, e2) -> type_binop info env op e1 e2
  | Eunop (op, e) -> type_unop info env op e
  | Eattr (e, id) ->
     let info, t, te = type_expr info env e in
     resolve_attr_type info env id te e t.t

  (* primitives function calls *)
  | Ecall(Some pkg, f, _) when pkg.v = "fmt" && f.v <> "Print" ->
     hint_error f.position (sprintf "unknown function `fmt.%s`" f.v)
       "did you mean `fmt.Print` ?"
  | Ecall(Some pkg, _, ps) when pkg.v = "fmt" ->
     (* TODO : dont lookup each time *)
     if not (Vset.mem "fmt" env.packages)
     then unknown_pkg env pkg.position "fmt";
     let info, ps =
       List.fold_right
         (fun p (info, ps) ->
           let info, _, te = type_expr info env p in
           info, te :: ps)
         ps (info, []) in
     used_pkg info "fmt", typ Tvoid, Tprint ps
  | Ecall (None, f, ps) when not (Smap.mem "new" env.funcs) && f.v = "new" ->
     begin match ps with
     | x :: [] ->
        begin match x.v with
        | Eident s when Smap.mem s env.types ->
           let t = Smap.find s env.types in
           info, typ (Tref t), Tnew t
        | Eident "int" -> info, typ (Tref Tint), Tnew Tint
        | Eident "string" -> info, typ (Tref Tstring), Tnew Tstring
        | Eident "bool" -> info, typ (Tref Tbool), Tnew Tbool
        | Eident s -> unknown_type env x.position s
        | Eattr ({ v = Eident pkg; _ }, s) when Vset.mem pkg env.packages->
           let pkg = Smap.find pkg !all_packages in
           begin try let t = Smap.find s.v pkg.types in
                     info, typ (Tref t), Tnew t
                 with Not_found -> unknown_type pkg s.position s.v end
        | Eattr ({ v = Eident pkg; position = pos }, _) ->
           unknown_pkg env pos pkg
        | _ -> compile_error x.position
                "`new` expects a struct or a primitive type as argument"
        end
     | _ -> compile_error f.position "`new` expects only one arguments"
     end

  (* general function call *)
  | Ecall (pkg, f, ({ v = Ecall(_, _, _); _ } as c) :: []) ->
     let info, t, te = type_expr info env c in
     let info, (ps_t, ret_t) = find_func info env pkg f in
     let ret_t = ret ret_t in
     begin match t.t, ps_t with
     | t, t' :: [] when typ_eq t t' ->
        info, typ ret_t, Tcall (func_name pkg f.v, te::[], ps_t, ret_t)
     | Ttuple ts, ts' when List.for_all2 typ_eq ts ts' ->
        info, typ ret_t, Tcall (func_name pkg f.v, te::[], ps_t, ret_t)
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
         let pkg_info = pkg_info info pkg in
         let func_pos = Smap.find f.v pkg_info.func_pos in
         args_nb_error func_pos f.position f.v
           (List.length ps_t) (List.length ps)
     in
     let tau = ret ret_t in
     info, typ tau, Tcall(func_name pkg f.v, tps, ps_t, tau)

(** Add a new variable of type ty to the environnement and
    remember in info that this is a local variable *)
and add_vars ty (info, ids, env) id =
  if id.v = "_"
  then info, id.v :: ids, env
  else try let pos = Smap.find id.v info.local_vars in
           already_declared pos id.position id.v
       with Not_found ->
         decl info id.v id.position,
         id.v :: ids,
         add_env id.v ty env

and add_vars2 acc ty id = add_vars ty acc id

and is_nil = function
  | Tenil -> true
  | Tetuple ts -> List.exists ((=) Tenil) ts
  | _ -> false

(** Auxiliary function handling variables declaration *)
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
               type_unexpected vs.position t expect
           with Not_found ->
             let info, ids, env =
               List.fold_left (add_vars expect) (info, [], env) ids in
             info, env, Tdecl (ids, expect, Some te)
     end
  | None, _ when is_nil te -> untyped_nil vs.position
  | None, ((Ttuple ts) as t) ->
     begin try let info, ids, env =
                 List.fold_left2 add_vars2 (info, [], env) ts ids in
         info, env, Tdecl (ids, t, Some te)
     with Invalid_argument _ ->
       decl_nb_error vs.position (List.length ts) (List.length ids)
     end
  | None, t ->
     let info, ids, env =
       List.fold_left (add_vars t) (info, [], env) ids in
     info, env, Tdecl (ids, t, Some te)
  | Some expect, t when typ_eq t expect ->
     let info, ids, env =
       List.fold_left (add_vars expect) (info, [], env) ids in
     info, env, Tdecl (ids, expect, Some te)
  | Some expect, t -> type_unexpected vs.position t expect

and check_return_no_underscore te =
  match te with
  | Tetuple ts -> List.exists check_return_no_underscore ts
  | Tident ("_", _) -> true
  | _ -> false

(** Type an instruction and returns
    - informations about the current state of the typing
    - a possibly modified environnement
    - a typed syntax tree **)
and type_instruction info env = function
  | Inop -> info, env, Tnop
  | Iexpr e ->
     let info, t, te = type_expr info env e in
     info, env, Texpr (te, t.t)
  | Idecl (ids, Some ty, None) ->
     let t = of_ty env ty.v in
     let info, ids, env =
       List.fold_left
         (add_vars t)
         (info, [], env) ids
     in
     info, env, Tdecl (ids, t, None)
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
     | _ -> info, env, Tasgn (te1, te2, t1.t) end
  | Iif (cond, i1, i2) ->
     let info, t, te = type_expr info env cond in
     begin match t.t with
     | Tbool ->
        let info1, _, b1 = type_instruction info env i1 in
        let info, _, b2 = type_instruction
                            { info1 with is_return = false } env i2 in
        { info with is_return = info1.is_return && info.is_return },
        env, Tif (te, b1, b2)
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
     | [] ->
        if t.t <> Tvoid
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
         if !allow_unused_var
         then begin warn "unused variable %s.@." v;
                    merge info sinfo, env, Tblock (List.rev tis)
              end
         else unused_var pos v
     with Not_found -> merge info sinfo, env, Tblock (List.rev tis)

(** Check that function declaration is correct **)
let rec check_function_params env = function
  | [] -> []
  | (ty, x) :: xs ->
     match List.find_opt (fun (_, y) -> y.v = x.v) xs with
     | Some (_, y) -> redondant_param_name y.position x.position x.v
     | None -> (x.v, of_ty env ty.v) :: (check_function_params env xs)

let rec check_function_return env = function
  | [] -> []
  | t :: ts -> (of_ty env t.v) :: check_function_return env ts

(** Check that structure declaration is correct *)
let type_structure env s =
  let rec check_fields = function
    | [] -> []
    | (ty, x) :: xs ->
       match List.find_opt (fun (_, y) -> y.v = x.v) xs with
       | Some (_, y) -> redondant_field_name y.position x.position x.v
       | None -> (x.v, (of_ty env ty.v)) :: check_fields xs
  in
  let fields = check_fields s.v.s_body in
  dbg "Add struct `%s` with %d fields to package `%s`@."
    s.v.s_name.v (List.length fields) !cur_pkg;
  { env with types = Smap.add s.v.s_name.v (Tstruct s.v.s_name.v) env.types;
             structs = Smap.add s.v.s_name.v fields env.structs }

(** Check that function is well-typed**)
let type_function (info, env) f =
  let (_, ret_type) = Smap.find f.v.f_name.v env.funcs in
  let finfo, vars =
    List.fold_left (fun (finfo, vars) (ty, id) ->
        decl finfo id.v id.position, Smap.add id.v (of_ty env ty.v) vars)
      ({ info with return_type = ret_type }, Smap.empty) f.v.f_params
  in
  let finfo, _, fbody = type_instruction finfo
                      { env with vars = vars } f.v.f_body in
  if ret_type <> [] && not finfo.is_return
  then compile_error f.position "this function may return nothing";
  dbg "Add function `%s` to the package.@." f.v.f_name.v;
  { info with used_pkg = Vset.union finfo.used_pkg info.used_pkg },
  { env with funcs_body = Smap.add f.v.f_name.v fbody env.funcs_body }

let type_prog env prog =
  dbg "Typing %s.@." !ifile;
  cur_pkg := prog.p_name.v;

  let info, env =
    List.fold_left
      (fun (info, env) pkg ->
        if pkg.v = "fmt"
        then begin
            dbg "Import package `fmt`@.";
            { info with pkg_pos = Smap.add pkg.v pkg.position info.pkg_pos },
            { env with packages = Vset.add "fmt" env.packages }
          end
        else try
            dbg "Import package `%s`@." pkg.v;
            { info with pkg_pos = Smap.add pkg.v pkg.position info.pkg_pos },
            { env with packages = Vset.add pkg.v env.packages }
          with Not_found -> unknown_import_pkg pkg.position pkg.v)
      (empty_info, env) prog.p_imports in

  (* Add structures without content *)
  let (info, env) =
    List.fold_left
      (fun (info, env) s ->
        try let _ = Smap.find s.v.s_name.v env.types in
            try
              let s_pos = Smap.find s.v.s_name.v info.struct_pos in
              struct_already_exists s_pos s.v.s_name.position s.v.s_name.v
            with Not_found -> compile_error s.position
                               "is already defined in an other package";
        with Not_found ->
          dbg "Check structure `%s`@." s.v.s_name.v;
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
          dbg "Check function %s%a@." f.v.f_name.v pp_func (ps, ret);
          { info with
            func_pos = Smap.add f.v.f_name.v f.position info.func_pos },
          { env with
            funcs = Smap.add f.v.f_name.v (ps, ret) env.funcs })
      (info, env) (List.rev prog.p_functions) in

  (* Add structure content *)
  let env = List.fold_left type_structure env prog.p_structures in

  (* Check cycles in types *)
  dbg "Construct structure inclusion graph with %d vertices.@."
    (Smap.cardinal env.structs);

  let g = List.map (fun s -> vertex s.v.s_name.v) prog.p_structures in
  List.iter (fun s ->
      match Smap.find s.v.s_name.v env.types with
      | Tstruct s ->
         List.iter
           (fun (_, t) ->
             match t with
             | Tstruct s' -> add_node g s' s
             | _ -> ()) (Smap.find s env.structs)
      | _ -> assert false)
    prog.p_structures;

  let order = topological_sort g in
  List.iteri (fun i s -> dbg "%d: %s@." i s) order;

  (* Type functions *)
  let info, env =
    List.fold_left type_function
      (info, env) prog.p_functions in

  (* Look for unused packages *)
  Smap.iter (fun p pos ->
      if not (Vset.mem p info.used_pkg)
      then if !allow_unused_package
           then warn "unused package %s@." p
           else unused_pkg pos p)
         info.pkg_pos;

  all_info_packages := Smap.add prog.p_name.v info !all_info_packages;

  { env with order = order }
