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

let structs : (ident, cstruct) Hashtbl.t = Hashtbl.create 16

let rec sizeof = function
  | Tnil -> assert false
  | Tvoid -> 0
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
  let prev = ref Tstring in
  let buf = Buffer.create 10 in
  let typs, es =
    List.fold_left (fun (typs, es) p ->
        match p with
        | Tenil -> Buffer.add_string buf "<nil>"; typs, es
        | Teint i ->
           if !prev <> Tstring then Buffer.add_char buf ' ';
           Buffer.add_string buf (Int64.to_string i);
           prev := Tint; typs, es
        | Testring s -> escaped_string buf s; prev := Tstring; typs, es
        | Tebool b ->
           if !prev <> Tstring then Buffer.add_char buf ' ';
           Buffer.add_string buf (string_of_bool b);
           prev := Tbool; typs, es
        | Tident (_, t) | Tattr (_, _, _, t) | Tunop (_, _, t)
          | Tbinop (_, _, _, t) | Tcall (_, _, _, _, t) | Tnew t ->
           if t <> Tstring && !prev <> Tstring
           then Buffer.add_char buf ' ';
           Buffer.add_string buf (type_to_format t);
           prev := t; t :: typs, p :: es
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

  (* type des parametres, type de retour *)
| Ccall    of ident * cexpr list * typ * typ
| Cunop    of unop * cexpr
| Cbinop   of binop * cexpr * cexpr
| Cprint   of (cexpr * typ) list * sident
| Cnew     of int * typ

type cinstruction =
  Cnop
| Cexpr    of cexpr
| Casgn    of cexpr * cexpr
| Cdefault of sident list * typ
| Cdecl    of sident list * cexpr * typ
| Cblock   of cinstruction list
| Creturn  of cexpr * int * int
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
  | Tcall (None, fname, params, ps, ret) ->
     Ccall (fname, List.map (build_expr env) params, Ttuple ps, ret)

  | Tcall _ -> assert false

and add (env, ids, next) t id =
  let sz = sizeof t in
  let next = sz + next in
  dbg "Add local var `%s` at offset %d (size %d).@." id (-next) sz;
  Smap.add id (-next) env, (-next) :: ids, next

and add_vars env ids next = function
  | Ttuple ts -> List.fold_left2 add (env, [], next) ts ids
  | t -> List.fold_left (fun acc id -> add acc t id) (env, [], next) ids

and build_instruction ofsp ofsr env next = function
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
     env, Cdecl (ids, ce, t), next

  | Tasgn (e1, e2) ->
     let ce1 = build_expr env e1 in
     let ce2 = build_expr env e2 in
     env, Casgn (ce1, ce2), next

  | Tif (e, i1, i2) ->
     let ce = build_expr env e in
     let env, ci1, next = build_instruction ofsp ofsr env next i1 in
     let env, ci2, next = build_instruction ofsp ofsr env next i2 in
     env, Cif(ce, ci1, ci2), next

  | Tfor (e, i) ->
     let ce = build_expr env e in
     let env, ci, next = build_instruction ofsp ofsr env next i in
     env, Cfor (ce, ci), next

  | Tblock is ->
     let env, cis, sz =
       List.fold_left
         (fun (env, cis, sz) e ->
           let env, ce, s = build_instruction ofsp ofsr env sz e in
           env, ce :: cis, s) (env, [], next) is
     in
     env, Cblock (List.rev cis), sz

  | Treturn e -> env, Creturn (build_expr env e, ofsp, ofsr), next

and escaped_mem esc i =
  let rec aux esc = function
    | Tunop (Ref, Tident (_, Tref _), _)
      | Tunop (Ref, Tident (_, Tstring), _)
      | Tunop (Ref, Tattr (Tident (_, Tstring), _, _, _), _)
      | Tunop (Ref, Tattr (Tident (_, Tref _), _, _, _), _) -> esc
    | Tunop (Ref, Tident (id, _), _)
      | Tunop (Ref, Tattr (Tident (id, _), _, _, _), _) ->
       Vset.add id esc
    | Tetuple es | Tcall (_, _, es, _, _) | Tprint es ->
       List.fold_left aux esc es
    | Tattr (e, _, _, _) | Tunop(_, e, _) -> aux esc e
    | Tbinop(_, e1, e2, _) -> aux (aux esc e1) e2
    | _ -> esc
  in
  match i with
  | Texpr e | Tdecl (_, _, Some e) | Treturn e -> aux esc e
  | Tnop | Tdecl _ -> esc
  | Tasgn (e1, e2) -> aux (aux esc e1) e2
  | Tblock is -> List.fold_left escaped_mem esc is
  | Tfor (e, i) -> escaped_mem (aux esc e) i
  | Tif (e, i1, i2) -> escaped_mem (escaped_mem (aux esc e) i1) i2

and escape_expr esc = function
  | Tident (id, t) as e ->
    if Vset.mem id esc then Tunop (Deref, e, t) else e
  | Tetuple es -> Tetuple (List.map (escape_expr esc) es)
  | Tattr (e, s, i, t) -> Tattr (escape_expr esc e, s, i, t)
  | Tcall (pkg, fname, ps, params, ret) ->
     Tcall (pkg, fname, List.map (escape_expr esc) ps, params, ret)
  | Tunop (Ref, (Tident (id, _) as e), _) when Vset.mem id esc -> e
  | Tunop (op, e, t) -> Tunop (op, escape_expr esc e, t)
  | Tbinop (op, e1, e2, t) -> Tbinop (op, escape_expr esc e1,
                                     escape_expr esc e2, t)
  | Tprint es -> Tprint (List.map (escape_expr esc) es)
  | t -> t

and option_map f = function Some e -> Some (f e) | None -> None

and escape_instr esc = function
  | Tnop -> Tnop
  | Texpr e -> Texpr (escape_expr esc e)
  | Tasgn (e1, e2) -> Tasgn (escape_expr esc e1, escape_expr esc e2)
  | Tblock is -> Tblock (List.map (escape_instr esc) is)
  | Tdecl (ids, t, v) ->
     let v = option_map (escape_expr esc) v in
     Tdecl (ids, t, v)
  | Treturn e -> Treturn (escape_expr esc e)
  | Tfor (e, i) -> Tfor (escape_expr esc e, escape_instr esc i)
  | Tif (e, i1, i2) -> Tif (escape_expr esc e,
                           escape_instr esc i1,
                           escape_instr esc i2)

and pp_list fmt = function
  | [] -> ()
  | x :: [] -> fprintf fmt "`%s`" x
  | x :: xs -> fprintf fmt "`%s`, %a" x pp_list xs

let build env =
  Smap.iter
    (fun name s ->
      let cs = compile_struct s in
      dbg "Compile structure `%s` (size %d).@." name cs.size;
      Hashtbl.add structs name cs) env.structs;

  Smap.mapi
    (fun fname body ->
      dbg "Compiling `%s`.@." fname;

      let esc = escaped_mem Vset.empty body in
      let body =
        if Vset.cardinal esc = 0 then body
        else begin
            dbg "Escape variables: %a.@." pp_list (Vset.elements esc);
            escape_instr esc body
          end
      in

      let params, ret = Smap.find fname env.funcs in
      let ofsp, env =
        List.fold_right
          (fun (id, t) (i, env) ->
            let sz = sizeof t in
            let ofs = i + sz in
            dbg "Add new parameter `%s` at offset %d (size %d).@." id i sz;
            ofs, Smap.add id i env)
          params (16, Smap.empty) in

      let ofsr = sizeof (Ttuple ret) in
      let env, cast, fsz = build_instruction ofsp ofsr env 0 body in

      let heap_alloc =
        Smap.fold (fun id ofs alloc ->
            if Vset.mem id esc then Iset.add ofs alloc else alloc)
          env Iset.empty
      in

      dbg "done (frame size %d).\n@." fsz;
      heap_alloc, cast, fsz)
    env.funcs_body
