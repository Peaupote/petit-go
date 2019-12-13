open Format
open Config
open Ast
open X86_64

(** Stack ident *)
type sident = int

type cstruct = {
    size : int;
    fields : sident Smap.t
  }

let structs : (ident, cstruct) Hashtbl.t = Hashtbl.create 10

let rec sizeof = function
  | Tvoid | Tnil -> assert false
  | Tstruct s -> let cstruct = Hashtbl.find structs s in cstruct.size
  | Tint | Tbool | Tstring | Tref _ -> 8
  | Ttuple ts -> List.fold_left (fun sum t -> (sizeof t) + sum) 0 ts

let compile_struct s =
  let size, fields =
    Smap.fold (fun id t (sz, cstruct) ->
        sizeof t + sz, Smap.add id sz cstruct)
      s (0, Smap.empty)
  in
  { size = size; fields = fields }


module type Cmp = sig
  type t
  val compare : t -> t -> int
end

module MakeSym (M : Cmp) = struct
  include Hashtbl

  let prefix = ref ""

  let lab x = sprintf ".%s_%d" !prefix x

  let create p n = prefix := p; create n
  let stored : (string, int) Hashtbl.t = Hashtbl.create 10

  let c = ref 0
  let add tbl v =
    try let c = find stored v in
        dbg "%s already in .data, don't create new symbol@."
          (String.capitalize_ascii !prefix);
        c
    with Not_found ->
      incr c;
      dbg "Add %s `%s` in .data.@." !prefix (lab !c);
      replace tbl !c v;
      replace stored v !c;
      !c

  let symbols tbl =
    fold (fun x s l -> label (lab x) ++ string s ++ l) tbl nop

end

module SSym = MakeSym(String)
module FSym = MakeSym(String)

let strings : (int, string) SSym.t = SSym.create "string" 10
let formats : (int, string) FSym.t = FSym.create "format" 10

let empty_string _ =
  let id = ref (SSym.add strings "") in
  SSym.lab !id

let true_string _ =
  let id = ref (SSym.add strings "true") in
  SSym.lab !id

let false_string _ =
  let id = ref (SSym.add strings "false") in
  SSym.lab !id

let nil_string _ =
  let id = ref (SSym.add strings "<nil>") in
  SSym.lab !id

(** Create a format *)
let escaped_string buf s =
  String.iter (fun c ->
      if c = '%'
      then Buffer.add_string buf "%%"
      else Buffer.add_char buf c) s

let type_to_format = function
  | Tvoid -> assert false
  | Tnil -> assert false
  | Tint -> "%d"
  | Tbool -> "%s"
  | Tstring -> "%s"
  | Ttuple _ -> assert false
  | Tstruct id -> id ^ "@{TODO}"
  | Tref _ -> "%s"

let make_format ps =
  let buf = Buffer.create 10 in
  let typs, es =
    List.fold_left (fun (typs, es) p ->
        match p with
        | Tenil -> Buffer.add_string buf "nil"; typs, es
        | Teint i -> Buffer.add_string buf (Int64.to_string i); typs, es
        | Testring s -> escaped_string buf s; typs, es
        | Tebool b -> Buffer.add_string buf (string_of_bool b); typs, es
        | Tident (_, t) | Tattr (_, _, _, t) | Tunop (_, _, t)
          | Tbinop (_, _, _, t) | Tcall (_, _, _, t) | Tnew t ->
           Buffer.add_string buf (type_to_format t);
           t :: typs, p :: es
        | Tetuple _ | Tprint _ -> assert false) ([], []) ps
  in
  List.rev typs, List.rev es, Buffer.contents buf

type cexpr =
  Cnil
| Cint     of int64
| Cstring  of int
| Cbool    of bool
| Cident   of sident
| Ctuple   of cexpr list
| Cattr    of cexpr * sident
| Ccall    of sident * cexpr list
| Cunop    of unop * cexpr
| Cbinop   of binop * cexpr * cexpr
| Cprint   of (cexpr * typ) list * sident
| Cnew     of int * typ

type cinstruction =
  Cnop
| Cexpr    of cexpr
| Casgn    of cexpr * cexpr
| Cdefault of sident list * typ
| Cdecl    of sident list * cexpr
| Cblock   of cinstruction list
| Creturn  of cexpr
| Cfor     of cexpr * cinstruction
| Cif      of cexpr * cinstruction * cinstruction

type sz_cinstruction = cinstruction * int

let rec build_expr env = function
  | Tenil -> Cnil
  | Teint i -> Cint i
  | Testring s -> Cstring (SSym.add strings s)
  | Tebool b -> Cbool b

  | Tident (id, _) -> Cident (Smap.find id env)
  | Tetuple es -> Ctuple (List.map (build_expr env) es)

  | Tattr (e, sname, id, _) ->
     let ce = build_expr env e in
     let cstruct = Hashtbl.find structs sname in
     Cattr (ce, Smap.find id cstruct.fields)

  | Tbinop (op, e1, e2, _) ->
     let ce1 = build_expr env e1 in
     let ce2 = build_expr env e2 in
     Cbinop (op, ce1, ce2)

  | Tunop (op, e, _) -> Cunop (op, build_expr env e)

  | Tprint es ->
     let typs, es, fmt = make_format es in
     let fmt = FSym.add formats fmt in
     let ces =
       List.fold_left2
         (fun (es) t e -> (build_expr env e, t) :: es) [] typs es
     in
     Cprint (ces, fmt)

  | Tnew t -> Cnew (sizeof t, t)

  | Tcall _ -> assert false

and add (env, ids, next) t id =
  let sz = sizeof t in
  let next = sz + next in
  dbg "Add local var `%s` (size %d).@." id sz;
  Smap.add id (-next) env, (-next) :: ids, next

and add_vars env ids next = function
  | Ttuple ts -> List.fold_left2 add (env, [], next) ts ids
  | t -> List.fold_left (fun acc id -> add acc t id) (env, [], next) ids

and build_instruction env next = function
  | Tnop -> env, Cnop, next
  | Texpr e ->
     let ce = build_expr env e in
     env, Cexpr ce, next

  | Tdecl (ids, t, None) ->
     let env, ids, next = add_vars env ids next t in
     env, Cdefault (ids, t), next

  | Tdecl (ids, t, Some e) ->
     let ce = build_expr env e in
     let env, ids, next = add_vars env ids next t in
     env, Cdecl (ids, ce), next

  | Tasgn (e1, e2) ->
     let ce1 = build_expr env e1 in
     let ce2 = build_expr env e2 in
     env, Casgn (ce1, ce2), next

  | Tif (e, i1, i2) ->
     let ce = build_expr env e in
     let env, ci1, next = build_instruction env next i1 in
     let env, ci2, next = build_instruction env next i2 in
     env, Cif(ce, ci1, ci2), next

  | Tfor (e, i) ->
     let ce = build_expr env e in
     let _, ci, next = build_instruction env next i in
     env, Cfor (ce, ci), next

  | Tblock is ->
     let _, cis, sz =
       List.fold_left
         (fun (env, cis, sz) e ->
           let env, ce, s = build_instruction env sz e in
           env, ce :: cis, s) (env, [], next) is
     in
     env, Cblock (List.rev cis), sz

  | _ -> assert false
