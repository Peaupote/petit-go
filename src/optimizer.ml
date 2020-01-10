open Ast
open Builder

(** Functions performing some simple compile-time optimization *)

let rec opt_add e1 e2 =
  match e1, e2 with
  | Cint x, Cint y -> Cint (Int64.add x y)
  | Cint 0L, e | e, Cint 0L -> e
  | e1, e2 -> Cbinop (Add, e1, e2, Tint)

and opt_sub e1 e2 = match e1, e2 with
  | Cint x, Cint y -> Cint (Int64.sub x y)
  | e1, e2 -> Cbinop (Sub, e1, e2, Tint)

and opt_not = function
  | Cbool b -> Cbool (not b)
  | e -> e

and opt_and e1 e2 = match e1, e2 with
  | Cbool false, _ -> Cbool false
  | Cbool true, e | e, Cbool true -> e
  | _ -> Cbinop (And, e1, e2, Tbool)

and opt_or e1 e2 = match e1, e2 with
  | Cbool false, e | e, Cbool false -> e
  | Cbool true, _ -> Cbool true
  | e1, e2 -> Cbinop(Or, e1, e2, Tbool)

and opt_expr e =
  match e with
  | Cbinop (Add, e1, e2, _) -> opt_add (opt_expr e1) (opt_expr e2)
  | Cbinop (Sub, e1, e2, _) -> opt_sub (opt_expr e1) (opt_expr e2)
  | Cbinop (And, e1, e2, _) -> opt_and (opt_expr e1) (opt_expr e2)
  | Cbinop (Or, e1, e2,  _) -> opt_or  (opt_expr e1) (opt_expr e2)
  | Cunop  (Not, e)         -> opt_not (opt_expr e)
  | _ -> e

let rec optimize = function
  | Cexpr (e, t) -> Cexpr (opt_expr e, t)
  | Casgn (ce1, ce2, t) -> Casgn (opt_expr ce1, opt_expr ce2, t)
  | Cdecl (ids, ce, t) -> Cdecl (ids, opt_expr ce, t)
  | Cblock is -> Cblock (List.map optimize is)
  | Creturn (ce, i, j) -> Creturn (opt_expr ce, i, j)

  | Cfor (ce, i) ->
     begin match opt_expr ce with
     | Cbool false -> Cnop
     | ce -> Cfor (ce, optimize i)
     end

  | Cif (ce, i1, i2) ->
     begin match opt_expr ce with
     | Cbool true -> optimize i1
     | Cbool false -> optimize i2
     | ce -> Cif (ce, optimize i1, optimize i2)
     end

  | i -> i
