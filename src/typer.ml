open Format
open Ast

module Smap = Map.Make(String)

type typ =
  Tint
| Tbool
| Tstring
| Tstruct of typ Smap.t
| Tref    of typ

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
| Tint    of int64
| Tstring of string
| Tbool   of bool
| Tident  of ident
| Ttuple  of texpr list
| Tattr   of texpr * ident
| Tcall   of texpr * texpr list
| Tunop   of unop * texpr
| Tbinop  of binop * texpr * texpr

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
let type_unexpected t expect =
  (* TODO : print types *)
  let msg =
    sprintf
      "This expression has type _ but an expression of type %s was expected"
      expect
  in
  typing_error t.position msg

let rec of_ty env = function
  | Ast.Tref t -> Tref (of_ty env t.v)
  | Ast.Tstruct { v = "int" ; _ } -> Tint
  | Ast.Tstruct { v = "bool" ; _ } -> Tbool
  | Ast.Tstruct { v = "string" ; _ } -> Tstring
  | Ast.Tstruct s ->
     try Smap.find s.v env.types
     with Not_found -> unknown_type s

let rec type_expr env expr =
  let type_list = List.map (fun e -> type_expr env e.v)  in
  match expr with
  | Enil -> Tnil
  | Eident id -> Tident (Smap.find id env)
  | Eint i -> Tint i
  | Ebool b -> Tbool b
  | Estring s -> Tstring s
  | Etuple es -> Ttuple (type_list es)
  | Eattr (e, attr) ->
     (* TODO : check if type t has that attribute *)
     begin match type_expr env e.v with
     | Tident _ as t -> Tattr (t, attr.v)
     | _ -> typing_error e.position "Can't take attribute of non-variable" end
  | Ecall (e, ps) ->
     (* TODO : check if parameters are the right type *)
     begin match type_expr env e.v with
     | Tident _ as t -> Tcall (t, type_list ps)
     | _ -> typing_error e.position "Not a function" end
  | Eunop (op, e) -> type_unop env op e
  | Ebinop (op, e1, e2) -> type_binop env op e1 e2

and type_unop env op e =
  let e' = type_expr env e.v in
  match op with
  | Not ->
     begin match e' with
     | Tbool _ -> Tunop (Not, e')
     | _ -> type_unexpected e "bool" end
  | Deref ->
     begin match e' with
     | Tnil -> typing_error e.position "you can't dereference nil pointer"
     | t -> Tunop (Deref, t) end
  | Ref ->
     (* TODO : check if left-value *)
     Tunop (Ref, e')

and type_binop env op e1 e2 =
  let e1' = type_expr env e1.v in
  let e2' = type_expr env e2.v in
  match op with
  | Add | Sub | Mul | Div | Mod ->
     begin match e1', e2' with
     | Tint _, Tint _ -> Tbinop (Add, e1', e2')
     | _, Tint _ -> type_unexpected e1 "int"
     | _, _ -> type_unexpected e2 "int" end
  | Lt | Gt | Leq | Geq ->
     begin match e1', e2' with
     | Tint _, Tint _ -> Tbinop (Lt, e1', e2')
     | _, Tint _ -> type_unexpected e1 "int"
     | _, _ -> type_unexpected e2 "int" end
  | _ -> assert false

let type_instruction env _ins = ignore(env)

let rec check_function_params env = function
  | [] -> []
  | (_, x) :: xs when List.exists (fun (_, y) -> y.v = x.v) xs ->
     (* TODO : better time complexity ? *)
     compile_error x.position
       (sprintf "variable name %s is used more than once" x.v)
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
         (sprintf "field name %s is used more than once" x.v)
    | ({ v = Ast.Tstruct name; _ }, _) :: _ when name.v = s.v.s_name.v ->
       compile_error name.position
         (sprintf "infinite sized-stack type %s" name.v)
    | (ty, x) :: xs -> check_fields (Smap.add x.v (of_ty env ty.v) fs) xs
  in
  let fields = check_fields Smap.empty s.v.s_body in
  { env with types = Smap.add s.v.s_name.v (Tstruct fields) env.types }

let type_function env f =
  let vars =
    List.fold_left (fun vars (ty, id) -> Smap.add id.v (of_ty env ty.v) vars)
      Smap.empty f.v.f_params
  in
  let _ = type_instruction { env with vars =  vars } f.v.f_body in
  env

let type_prog env prog =
  (* Add structures without content *)
  let env =
    List.fold_left
      (fun env s ->
        if Smap.mem s.v.s_name.v env.types
        then compile_error s.v.s_name.position
               (sprintf "structure %s already exists" s.v.s_name.v);

        { env with
          types = Smap.add s.v.s_name.v (Tstruct Smap.empty) env.types })
      env prog.p_structures
  in

  (* Check and add functions *)
  let env =
    List.fold_left
      (fun env f ->
        if Smap.mem f.v.f_name.v env.funcs
        then compile_error f.v.f_name.position
               (sprintf "function %s already exists" f.v.f_name.v);

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
