open Format
open Config
open X86_64
open Ast

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

let com s = if !verbose then comment s else nop

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

(** Registers for parameters *)
let preg = [rdi; rsi; rdx; rcx; r8; r9]

(** Create a format *)
let rec type_to_format = function
  | Tvoid -> assert false
  | Tnil -> assert false
  | Tint -> "%d"
  | Tbool -> "%d"
  | Tstring -> "%s"
  | Ttuple _ -> assert false
  | Tstruct id -> id ^ "@%p"
  | Tref typ -> type_to_format typ

let make_format ps =
  let buf = Buffer.create 10 in
  List.iter (fun p ->
      match p with
      | Tenil -> Buffer.add_string buf "nil"
      | Teint i -> Buffer.add_string buf (Int64.to_string i)
      | Testring s -> Buffer.add_string buf s
      | Tebool b -> Buffer.add_string buf (string_of_bool b)
      | Tident (_, t) | Tattr (_, _, t) | Tunop (_, _, t)
        | Tbinop (_, _, _, t) | Tcall (_, _, _, t) | Tnew t ->
         Buffer.add_string buf (type_to_format t)
      | Tetuple _ | Tprint _ -> assert false) ps;
  Buffer.contents buf

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
| Cprint   of cexpr list * sident
| Cnew     of cstruct

type cinstruction =
  Cnop
| Cexpr   of cexpr
| Casgn   of cexpr * cexpr
| Cdecl   of sident list * cexpr
| Cblock  of cinstruction list
| Creturn of cexpr
| Cfor    of cexpr * cinstruction
| Cif     of cexpr * cinstruction * cinstruction

type sz_cinstruction = cinstruction * int

let rec build_expr env next = function
  | Tenil -> Cnil, next
  | Teint i -> Cint i, next
  | Testring s -> Cstring (SSym.add strings s), next
  | Tebool b -> Cbool b, next

  | Tident (id, _) -> Cident (Smap.find id env), next
  | Tetuple es ->
     let ces, sz =
       List.fold_left
         (fun (ces, sz) e ->
           let ce, s = build_expr env sz e in
           ce :: ces, max s sz) (* max maybe useless *)
         ([], next) es
     in
     Ctuple (List.rev ces), sz

  | Tbinop (op, e1, e2, _) ->
     let ce1, sz1 = build_expr env next e1 in
     let ce2, sz2 = build_expr env next e2 in
     Cbinop (op, ce1, ce2), max sz1 sz2

  | Tunop (op, e, _) ->
     let ce, sz = build_expr env next e in
     Cunop (op, ce), sz

  | Tprint es ->
     let fmt = FSym.add formats (make_format es) in
     let ces, sz =
       List.fold_left
         (fun (es, sz) e ->
           let ce, s = build_expr env next e in
           ce :: es, max sz s) ([], 0) es
     in
     Cprint (ces, fmt), sz

  | _ -> assert false

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
     let ce, sz = build_expr env next e in
     env, Cexpr ce, sz

  | Tdecl (ids, t, None) ->
     let env, _, next = add_vars env ids next t in
     env, Cnop, next

  | Tdecl (ids, t, Some e) ->
     let ce, next = build_expr env next e in
     let env, ids, next = add_vars env ids next t in
     env, Cdecl (ids, ce), next

  | Tasgn (e1, e2) ->
     let ce1, next = build_expr env next e1 in
     let ce2, next = build_expr env next e2 in
     env, Casgn (ce1, ce2), next

  | Tblock is ->
     let env, cis, sz =
       List.fold_left
         (fun (env, cis, sz) e ->
           let env, ce, s = build_instruction env sz e in
           env, ce :: cis, s) (env, [], next) is
     in
     env, Cblock (List.rev cis), sz

  | _ -> assert false

and compile_binop = function
  | Add -> addq !%rbx !%rax
  | Sub -> subq !%rbx !%rax
  | Mul -> imulq !%rbx !%rax
  | Div -> cqto ++ idivq !%rbx
  (* ! parameter in rdx *)
  (* TODO : shift when power of 2 *)
  | Mod -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
  | And | Eq -> andq !%rbx !%rax
  | Neq -> cmpq !%rbx !%rax ++ setne !%al
  | Or -> orq !%rbx !%rax
  | Lt -> cmpq !%rbx !%rax ++ setl !%al
  | Leq -> cmpq !%rbx !%rax ++ setle !%al
  | Gt -> cmpq !%rbx !%rax ++ setg !%al
  | Geq -> cmpq !%rbx !%rax ++ setge !%al

and compile_unop = function
  | Not -> testq (imm 1) !%rax ++ setne !%al
  | _ -> assert false

and push_params code reg es =
  match reg, es with
  | _, [] -> code ++ com "all args pushed"
  | [], e :: tl ->
     let ce = compile_expr e in
     let code =
       code ++ com "push arg onto stack" ++
         ce ++ pushq !%rax
     in
     push_params code reg tl
  | rd :: rtl, e :: es ->
     let ce = compile_expr e in
     let code =
       code ++ com "push arg into register" ++
         ce ++ movq !%rax !%rd
     in
     push_params code rtl es

and compile_expr = function
  | Cnil -> movq (imm 0) !%rax
  | Cint i -> movq (imm64 i) !%rax
  | Cbool true -> movq (imm 1) !%rax
  | Cbool false -> movq (imm 0) !% rax
  | Cstring c -> movq (ilab (SSym.lab c)) !%rax

  | Cident i -> movq (ind ~ofs:i rbp) !%rax

  | Ctuple es ->
     List.fold_left
       (fun code e -> code ++ compile_expr e ++ pushq !%rax)
       nop (List.rev es)
     ++ popq rax (* TODO : optimize *)

  | Cbinop (op, e1, e2) ->
     com "binop" ++
     compile_expr e2 ++
       movq !%rax !%rbx ++
       compile_expr e1 ++
       compile_binop op

  | Cunop (op, e) ->
     com "unop" ++
       compile_expr e ++
       compile_unop op

  | Cprint (es, fmt) ->
     let params_to_push =
       List.filter (fun e ->
           match e with
           | Cstring _ | Cbool _ | Cnil | Cint _ -> false
           | _ -> true) es in
     let params_to_push = List.rev params_to_push in
     push_params (com "push args of printf") (List.tl preg) params_to_push ++
       movq (ilab (FSym.lab fmt)) !%rdi ++
       movq (imm 0) !%rax ++ call "printf"

  | _ -> assert false

and compile_instruction = function
  | Cnop -> nop
  | Cexpr e -> compile_expr e

  | Casgn (e1, e2) ->
     let code = compile_expr e2 in
     code ++
     begin match e1 with
     | Cident id -> movq !%rax (ind ~ofs:id rbp)
     | _ -> assert false
     end

  | Cdecl (ids, ce) ->
     let more = ref false in (* TODO : something better *)
     let code = compile_expr ce in
     List.fold_left
       (fun code id ->
         (if !more then code ++ popq rax else (more := true; code)) ++
           movq !%rax (ind ~ofs:id rbp))
       code ids

  | Cblock es ->
     List.fold_left (fun code i -> code ++ compile_instruction i)
       (com "compile new block") es

  | _ -> assert false

let build env =
  Smap.map
    (fun body ->
      let _, cast, fsz = build_instruction Smap.empty 0 body in
      dbg "done (frame size %d).\n@." fsz;
      cast, fsz)
    env.funcs_body

let compile env =
  let funcs = build env in
  let cmain, cfuncs =
    Smap.fold (fun fname (body, fsz) (cmain, cfuncs) ->
        dbg "Generating assembly code for function `%s`.@." fname;
        if fname = "main"
        then pushn fsz ++
               compile_instruction body ++
               popn fsz, cfuncs
        else cmain,
             cfuncs ++
               label fname ++
               pushq !%rbp ++
               movq !%rsp !%rbp ++
               compile_instruction body ++
               popn fsz ++
               ret)
      funcs (nop, nop)
  in
  cmain, cfuncs

let gcc_command = "gcc -g -no-pie %s -o %s" ^^ ""

let compile_program compile_order =
  let code_main, code_funcs =
    Queue.fold
      (fun (code_main, code_funcs) pkg ->
        dbg "Start compiling `%s`@." pkg;
        let cmain, cfuncs = compile (Smap.find pkg !all_packages) in
        code_main ++ cmain, code_funcs ++ cfuncs)
      (nop, nop) compile_order
  in

  let p =
    { text =
        globl "main" ++ label "main" ++
          movq !%rsp !%rbp ++
          code_main ++
          movq (imm 0) !%rax ++ (* exit *)
          ret ++
          code_funcs;
      data =
        SSym.symbols strings ++
          FSym.symbols formats
    }
  in

  if !ofile = ""
  then ofile := (Filename.remove_extension !ifile);

  let asm_file =
    if not (!keep_asm)
    then Filename.temp_file "petitgo" (!ofile ^ ".s")
    else sprintf "%s.s" !ofile in

  dbg "Assembly code fully generated.@.";
  let f = open_out asm_file in
  let fmt = formatter_of_out_channel f in
  print_program fmt p;
  fprintf fmt "@?";
  close_out f;

  dbg "Compile assembly code with gcc.@.";
  let cmd = sprintf gcc_command asm_file !ofile in
  dbg "> %s@." cmd;

  let _ =
    try Sys.command cmd
    with Sys_error msg ->
      eprintf "An unexpected error occured while calling gcc: %s.@." msg;
      eprintf "Use option -v for more information.@.";
      exit 2
  in

  if !keep_asm
  then dbg "Option -S is up so keep assembly code in %s@." asm_file
  else begin dbg "Detele assembly file.@."; Sys.remove asm_file; end
