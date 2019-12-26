open Format
open Config
open X86_64
open Ast
open Builder

type compile_info = {
    heap_alloc : Iset.t;
    is_main : bool;
    frame_size : int; }

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
  | Eq -> xorq !%rbx !%rax ++ sete !%al
  | Neq -> xorq !%rbx !%rax ++ setne !%al
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

and resolve_name fname = if fname = "main_main" then "main" else fname

and reverse_stack _ts =
  assert false

and push_func_args ps ts =
  let push code p t =
    let sz = sizeof t in
    if sz <= 8 then code ++ compile_expr p ++ pushq !%rax
    else code ++ compile_expr ~push_value:true p in
  List.fold_left2 push (com "push args ") ps ts

and push_loop base sz rd =
  let rec aux code i =
    if i = 0 then code
    else aux (code ++ pushq (ind ~ofs:(base + i - 8) rd)) (i-8)
  in
  aux nop sz

and pop_loop sz rd =
  let rec aux code i =
    if i = sz then code
    else aux (code ++ popq rcx ++ movq !%rcx (ind ~ofs:i rd)) (i+8)
  in
  aux (com (sprintf "pop_loop %d" sz)) 0

(* push value everywhere *)
and compile_expr ?(push_value=false) = function
  | Cnil when push_value  -> pushq (imm 0)
  | Cint i  when push_value -> pushq (imm64 i)
  | Cbool true  when push_value -> pushq (imm 1)
  | Cbool false  when push_value -> pushq (imm 0)
  | Cstring c  when push_value -> pushq (ilab (SSym.lab c))

  | Cnil -> xorq !%rax !%rax
  | Cint i -> movq (imm64 i) !%rax
  | Cbool true -> movq (imm 1) !%rax
  | Cbool false -> xorq !%rax !%rax
  | Cstring c -> movq (ilab (SSym.lab c)) !%rax

  | Cident (i, t) when push_value || sizeof t > 8 -> push_loop i (sizeof t) rbp
  | Cident (i, _) -> movq (ind ~ofs:i rbp) !%rax

  | Cunop(Ref, ce) -> compile_left ce ++ if push_value then pushq !%rax else nop

  | Ctuple es ->
     List.fold_left (fun code e -> code ++ compile_expr ~push_value:true e)
       nop es

  | Cattr (ce, id, t) ->
     com (asprintf "attr %a" pp_typ t) ++
       compile_left ce ++
       if sizeof t > 8 then push_loop 0 (sizeof t) rax
       else movq (ind ~ofs:id rax) !%rax ++
              if push_value then pushq !%rax else nop

  | Cbinop(op, e1, e2) when op = And || op = Or ->
     let lend = nlab () in
     com "lazy op" ++
       compile_expr e1 ++
       xorq (imm 0) !%rax ++
       (if op = Or then jne else je) lend ++
       compile_expr e2 ++
       label lend ++
       if push_value then pushq !%rax else nop

  | Cbinop (op, e1, e2) ->
     com "binop" ++
       compile_expr e2 ++
       pushq !%rax ++
       compile_expr e1 ++
       popq rbx ++
       compile_binop op ++
       if push_value then pushq !%rax else nop

  | Cunop (op, e) ->
     com "unop" ++
       compile_expr e ++
       compile_unop op ++
       if push_value then pushq !%rax else nop

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
       | ce, Ttuple ts -> n + List.length ts,
                         code ++ compile_expr ce ++ reverse_stack ts
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
       movq (ilab (empty_string ())) (ind rax) ++
       if push_value then pushq !%rax else nop

  | Cnew (sz, t) ->
     com (asprintf "new %a" pp_typ t) ++
     movq (imm sz) !%rdi ++
       call "malloc" ++
       initialize_mem 0 rax t ++
       if push_value then pushq !%rax else nop

  (* if g returns in %rax, unefficient to compose this way *)
  | Ccall (fname, (Ccall (_, _, _, gret) as g) :: [], params, ret)
       when sizeof gret > 8 ->
     compile_expr g ++
       com "compose" ++
       call (resolve_name fname) ++
       (if sizeof ret <= 8
        then popn (sizeof (Ttuple params)) ++
               if push_value then pushq !%rax else nop
        else leaq (ind ~ofs:(8-sizeof ret) rcx) rsp) ++
       com "done call"

  | Ccall (fname, params, pt, ret) when sizeof ret <= 8 ->
     push_func_args params pt ++
       call (resolve_name fname) ++
       popn (sizeof (Ttuple pt)) ++
       com "done call" ++
       if push_value && sizeof ret = 8 then pushq !%rax else nop

  | Ccall (fname, params, pt, ret) ->
     push_func_args params pt ++
       call (resolve_name fname) ++
       leaq (ind ~ofs:(8-sizeof ret) rcx) rsp ++
       com "done call"

(* put write address in rax *)
and compile_left = function
  | Cident (id, _) ->
     leaq (ind ~ofs:id rbp) rax

  | Cattr (ce, id, _) ->
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

and write_value sz =
  if sz > 8 then pop_loop sz rax
  else popq rbx ++ movq !%rbx (ind rax)

and compile_instruction info = function
  | Cnop -> nop
  | Cexpr (e, t) ->
     let sz = sizeof t in
     if sz > 8 then compile_expr e ++ popn sz
     else compile_expr e

  | Casgn (Ctuple es, e2, Ttuple ts) ->
     let code = com (asprintf "asgn %a" pp_typ (Ttuple ts)) ++
                  compile_expr e2 in
     List.fold_right2
       (fun e t code -> code ++ compile_left e ++ write_value (sizeof t))
       es ts code

  | Cdecl (ids, ce, Ttuple ts) ->
     let code =
       com (asprintf "declare %a" pp_typ (Ttuple ts)) ++
         compile_expr ce in
     List.fold_right2
       (fun id t code -> code ++ popq rax ++
                          alloc_on_heap info.heap_alloc id t)
       ids ts code

  | Cdecl(id :: [], ce, t) ->
     com (asprintf "declare %a" pp_typ t) ++
       compile_expr ce ++
       alloc_on_heap info.heap_alloc id t

  | Cdecl _ -> assert false

  | Casgn (e1, e2, t) ->
     com (asprintf "asgn %a" pp_typ t) ++
       compile_expr ~push_value:true e2 ++
       compile_left e1 ++
       write_value (sizeof t)

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

let compile pkg env =
  let funcs = build env in
  let cmain, cfuncs =
    Smap.fold (fun fname (heap_alloc, body, afs) (cmain, cfuncs) ->
        dbg "Generating assembly code for function `%s.%s`.@." pkg fname;
        let ps = fst (Smap.find fname env.funcs) in
        let is_main = pkg = "main" && fname = "main" in
        let info = compile_info heap_alloc is_main afs in
        if is_main
        then pushn afs ++
               compile_instruction info body ++
               popn afs, cfuncs
        else cmain,
             cfuncs ++
               label (sprintf "%s_%s" pkg fname) ++
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
        let cmain, cfuncs = compile pkg (Smap.find pkg !all_packages) in
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
