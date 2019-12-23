open Format
open Config
open X86_64
open Ast
open Builder

type compile_info = {
    heap_alloc : Iset.t;
    is_main : bool;
    frame_size : int;
  }

let compile_info alloc is_main afz =
  { heap_alloc = alloc;
    is_main = is_main;
    frame_size = afz }

let com s = if !verbose then comment s else nop

(** Registers for parameters *)
let preg = [rdi; rsi; rdx; rcx; r8; r9]

let rec compile_binop = function
  | Add -> addq !%rbx !%rax
  | Sub -> subq !%rbx !%rax
  | Mul -> imulq !%rbx !%rax
  | Div -> cqto ++ idivq !%rbx

  (* TODO : shift when power of 2 *)
  | Mod -> cqto ++ idivq !%rbx ++ movq !%rdx !%rax
  | And -> andq !%rbx !%rax

  (* TODO : use xor instead *)
  | Eq -> cmpq !%rbx !%rax ++ sete !%al
  | Neq -> cmpq !%rbx !%rax ++ setne !%al
  | Or -> orq !%rbx !%rax
  | Lt -> cmpq !%rbx !%rax ++ setl !%al
  | Leq -> cmpq !%rbx !%rax ++ setle !%al
  | Gt -> cmpq !%rbx !%rax ++ setg !%al
  | Geq -> cmpq !%rbx !%rax ++ setge !%al

and compile_unop = function
  | Not -> testq !%rax !%rax ++ sete !%al
  | Deref -> movq (ind rax) !%rax
  | Ref -> assert false

and push_params code reg n =
  match reg, n with
  | _, 0 -> code ++ com "all args pushed"
  | [], _ -> code ++ com "all args onto stack"
  | rd :: rtl, n ->
     let code =
       code ++ com "push arg into register" ++
         popq rax ++ movq !%rax !%rd
     in
     push_params code rtl (n-1)

and nb_label = ref 0
and nlab () = incr nb_label; ".random_label" ^ (string_of_int !nb_label)

and compile_expr = function
  | Cnil -> xorq !%rax !%rax
  | Cint i -> movq (imm64 i) !%rax
  | Cbool true -> movq (imm 1) !%rax
  | Cbool false -> xorq !%rax !%rax
  | Cstring c -> movq (ilab (SSym.lab c)) !%rax
  | Cident i -> movq (ind ~ofs:i rbp) !%rax
  | Cunop(Ref, ce) -> compile_left ce

  | Ctuple es ->
     List.fold_left (fun code e -> code ++ compile_expr e ++ pushq !%rax)
       nop es

  | Cattr (ce, id) ->
     com "attr" ++
       compile_left ce ++
       movq (ind ~ofs:id rax) !%rax

  | Cbinop(op, e1, e2) when op = And || op = Or ->
     let lend = nlab () in
     com "lazy op" ++
       compile_expr e1 ++
       xorq (imm 0) !%rax ++
       (if op = Or then jne else je) lend ++
       compile_expr e2 ++
       label lend

  | Cbinop (op, e1, e2) ->
     com "binop" ++
       compile_expr e2 ++
       pushq !%rax ++
       compile_expr e1 ++
       popq rbx ++
       compile_binop op

  | Cunop (op, e) ->
     com "unop" ++
       compile_expr e ++
       compile_unop op

  | Cprint (es, fmt) ->
     let f (n, code) e =
       match e with
       | Cstring _, _ | Cbool _, _ | Cnil, _ | Cint _, _ -> n, code
       | ce, Tbool ->
          let el, l = nlab (), nlab () in
          let code =
            code ++ compile_expr ce ++
              xorq (imm 0) !%rax ++ je l ++
              pushq (ilab (true_string ())) ++
              jmp el ++ label l ++
              pushq (ilab (false_string ())) ++
              label el
          in
          n + 1, code
       | ce, Tref _ ->
          let el, l = nlab (), nlab () in
          let code =
            code ++ compile_expr ce ++
              xorq (imm 0) !%rax ++ je l ++
              pushq !%rax ++ jmp el ++ label l ++
              pushq (ilab (nil_string ())) ++
              label el
          in
          n + 1, code
       | ce, _ -> n + 1, code ++ compile_expr ce ++ pushq !%rax
     in
     let n, code = List.fold_left f (0, nop) es  in
     push_params (com "push args of printf" ++ code) (List.tl preg) n ++
       movq (ilab (FSym.lab fmt)) !%rdi ++
       xorq !%rax !%rax ++ call "printf"

  | Cnew (_, Tstring) ->
     com "new string" ++
       movq (imm 8) !%rdi ++
       call "malloc" ++
       movq (ilab (empty_string ())) (ind rax)

  | Cnew (sz, t) ->
     com (asprintf "new %a" pp_typ t) ++
     movq (imm sz) !%rdi ++
       call "malloc" ++
       initialize_mem 0 rax t

  (* if g returns in %rax, unefficient to compose this way *)
  | Ccall (fname, (Ccall (_, _, _, gret) as g) :: [], params, ret)
       when sizeof gret > 8 ->
     compile_expr g ++
       com "compose" ++
       call fname ++
       (if sizeof ret <= 8
        then popn (sizeof params)
        else leaq (ind ~ofs:(8-sizeof ret) rcx) rsp) ++
       com "done call"

  | Ccall (fname, params, _, ret) when sizeof ret <= 8 ->
     List.fold_left
       (fun code p -> code ++ compile_expr p ++ pushq !%rax)
       (com ("push args of " ^ fname)) params ++
       call fname ++
       popn (8 * List.length params) ++
       com "done call"

  | Ccall (fname, params, _, ret) ->
     List.fold_left
       (fun code p -> code ++ compile_expr p ++ pushq !%rax)
       (com ("push args of " ^ fname)) params ++
       call fname ++
       leaq (ind ~ofs:(8-sizeof ret) rcx) rsp ++
       com "done call"

(* put write address in rax *)
and compile_left = function
  | Cident id ->
     leaq (ind ~ofs:id rbp) rax

  | Cattr (ce, id) ->
     compile_left ce ++
       leaq (ind ~ofs:id rax) rax

  | Cunop(Deref, ce) ->
     compile_left ce ++
       movq (ind rax) !%rax

  | Cunop(Ref, ce) ->
     com "ref" ++
       compile_expr ce

  | _ -> assert false

and jump_if =
  let nb_if = ref 0 in
  let code info ce ci1 ci2 =
    incr nb_if;
    let lab = "_" ^ string_of_int !nb_if in
    com ("start if" ^ lab) ++
      compile_expr ce ++
      testq (imm 1) !%rax ++
      je (".else" ^ lab) ++
      compile_instruction info ci1 ++
      jmp (".endif" ^ lab) ++
      label (".else" ^ lab) ++
      compile_instruction info ci2 ++
      label (".endif" ^ lab)
  in
  code

and jump_loop =
  let nb_loop = ref 0 in
  let code info ce ci =
    incr nb_loop;
    let lab = "_" ^ string_of_int !nb_loop in
    com ("start loop" ^ lab) ++
      jmp (".end_loop" ^ lab) ++
      label (".loop" ^ lab) ++
      compile_instruction info ci ++
      label (".end_loop" ^ lab) ++
      compile_expr ce ++
      testq (imm 1) !%rax ++
      jne (".loop" ^ lab)
  in
  code

and initialize_mem ofs reg = function
  | Tint | Tbool | Tref _ ->
     movq (imm 0) (ind ~ofs:ofs reg)
  | (Tstruct _) as t ->
     let n = (sizeof t) / 8 in
     let rec aux i code =
       if i = n then code
       else aux (i+1) (code ++ movq (imm 0) (ind ~ofs:(ofs + 8 * i) reg)) in
     aux 0 nop
  | Tstring -> movq (ilab (empty_string ())) (ind ~ofs:ofs reg)
  | _ -> assert false

and alloc_heap id t =
  movq !%rax !%r15 ++ movq (imm (sizeof t)) !%rsi ++ call "malloc" ++
    movq !%r15 (ind rax) ++
    movq !%rax (ind ~ofs:id rbp)

and alloc_on_heap heap_alloc id t =
  if not (Iset.mem id heap_alloc)
  then movq !%rax (ind ~ofs:id rbp)
  else alloc_heap id t

and alloc_heap_params heap_alloc ps =
  let _, code =
    List.fold_right
      (fun (_, t) (i, code) ->
        let sz = sizeof t in
        let ofs = i + sz in
        let c = if Iset.mem i heap_alloc
                then alloc_heap i t
                else nop in
        ofs, code ++ c)
      ps (16, nop)
  in
  code

and compile_instruction info = function
  | Cnop -> nop
  | Cexpr e -> compile_expr e

  | Casgn (Ctuple es, e2) ->
     let code = com "asgn tuple" ++ compile_expr e2 in
     List.fold_right
       (fun e code -> code ++ compile_left e ++ popq rbx ++
                       movq !%rbx (ind rax))
       es code

  | Cdecl (ids, ce, Ttuple ts) ->
     List.fold_right2
       (fun id t code -> code ++ popq rax ++
                          alloc_on_heap info.heap_alloc id t)
       ids ts (com "declare" ++ compile_expr ce)

  | Cdecl(id :: [], ce, t) ->
     com "declare" ++
       compile_expr ce ++
       alloc_on_heap info.heap_alloc id t

  | Cdecl _ -> assert false

  | Casgn (e1, e2) ->
     com "asgn" ++
       compile_expr e2 ++
       pushq !%rax ++
       compile_left e1 ++
       popq rbx ++
       movq !%rbx (ind rax)

  | Cdefault (ids, t) ->
     List.fold_left
       (fun code id -> code ++ initialize_mem id rbp t)
       (com "default values") ids

  | Cif (ce, ci1, ci2) -> jump_if info ce ci1 ci2
  | Cfor (ce, ci) -> jump_loop info ce ci
  | Cblock es ->
     List.fold_left (fun code i -> code ++ compile_instruction info i)
       (com "compile new block") es

  | Creturn (_, _, 0) when info.is_main -> (* exit 0 *)
     movq !%rbp !%rsp ++
       xorq !%rax !%rax ++
       ret

  | Creturn (_, _, 0) -> (* return void *)
     movq !%rbp !%rsp ++
       popq rbp ++
       ret

  | Creturn (c, _, 8) -> (* return in %rax *)
     compile_expr c ++
       movq !%rbp !%rsp ++
       popq rbp ++
       ret

  | Creturn (ce, ofsp, ofsr) -> (* return on stack *)
     let rec write_ret pos =
       if pos = ofsr then nop
       else movq (ind ~ofs:(ofsr-pos-8) rsp) !%r15 ++
              movq !%r15 (ind ~ofs:(-pos) rcx) ++
              write_ret (pos + 8)
     in
     com "return" ++
       compile_expr ce ++
       leaq (ind ~ofs:(ofsp - 8) rbp) rcx ++(* point to first arg position *)
       movq (ind ~ofs:8 rbp) !%rdx ++       (* keep return address *)
       movq (ind rbp) !%rbp ++              (* keep old rbp *)
       com "write return values" ++
       write_ret 0 ++
       popn info.frame_size ++
       pushq !%rdx ++
       ret

let compile env =
  let funcs = build env in
  let cmain, cfuncs =
    Smap.fold (fun fname (heap_alloc, body, afs) (cmain, cfuncs) ->
        dbg "Generating assembly code for function `%s`.@." fname;
        let ps = fst (Smap.find fname env.funcs) in
        let info = compile_info heap_alloc (fname = "main") afs in
        if fname = "main"
        then pushn afs ++
               compile_instruction info body ++
               popn afs, cfuncs
        else cmain,
             cfuncs ++
               label fname ++
               pushq !%rbp ++
               movq !%rsp !%rbp ++
               pushn afs ++
               alloc_heap_params heap_alloc ps ++
               compile_instruction info body ++
               movq !%rbp !%rsp ++
               popq rbp ++
               ret)
      funcs (nop, nop)
  in
  cmain, cfuncs

let gcc_command () =
  "gcc -g -no-pie %s -o %s" ^^
    (if !verbose then "" else " 1&>2 2>/dev/null")

let compile_program compile_order =
  let code_main, code_funcs =
    Queue.fold
      (fun (code_main, code_funcs) pkg ->
        dbg "Start compiling package `%s`@." pkg;
        let cmain, cfuncs = compile (Smap.find pkg !all_packages) in
        code_main ++ cmain, code_funcs ++ cfuncs)
      (nop, nop) compile_order
  in

  let p =
    { text =
        globl "main" ++ label "main" ++
          movq !%rsp !%rbp ++
          code_main ++
          xorq !%rax !%rax ++ (* exit *)
          ret ++
          code_funcs;
      data =
        SSym.symbols strings ++
          FSym.symbols formats }
  in

  if !ofile = ""
  then ofile := (Filename.remove_extension !ifile);

  let asm_file = sprintf "%s.s" !ofile in

  dbg "Assembly code fully generated in %s.@." asm_file;
  let f = open_out asm_file in
  let fmt = formatter_of_out_channel f in
  print_program fmt p;
  fprintf fmt "@?";
  close_out f;

  if not !exec then exit 0;

  dbg "Compile assembly code with gcc.@.";
  let cmd = sprintf (gcc_command ()) asm_file !ofile in
  dbg "> %s@." cmd;

  try ignore(Sys.command cmd)
  with Sys_error _ ->
    eprintf "An unexpected error occured while calling gcc.@.";
    eprintf "For more information try using -v option.@.";
    exit 2
