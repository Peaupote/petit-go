open Format
open Config
open X86_64
open Ast
open Builder

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
  | Cbool false -> xorq !%rax !% rax
  | Cstring c -> movq (ilab (SSym.lab c)) !%rax

  | Cident i -> movq (ind ~ofs:i rbp) !%rax
  | Cunop(Ref, ce) -> compile_left ce

  | Ctuple es ->
     List.fold_left
       (fun code e -> code ++ compile_expr e ++ pushq !%rax)
       nop (List.rev es)
     ++ popq rax (* TODO : optimize *)

  | Cattr (ce, id) ->
     com "attr" ++
       compile_left ce ++
       movq (ind ~ofs:id rax) !%rax

  (* TODO : lazy if boolean operators *)
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
     let f e (n, code) =
       match e with
       | Cstring _, _ | Cbool _, _ | Cnil, _ | Cint _, _ -> n, code
       | ce, Tbool ->
          let el, l = nlab (), nlab () in
          let code =
            code ++ compile_expr ce ++
              xorq (imm 0) !%rax ++
              je l ++
              pushq (ilab (true_string ())) ++
              jmp el ++
              label l ++
              pushq (ilab (false_string ())) ++
              label el
          in
          n + 1, code
       | ce, Tref _ ->
          let el, l = nlab (), nlab () in
          let code =
            code ++ compile_expr ce ++
              xorq (imm 0) !%rax ++
              je l ++
              pushq !%rax ++
              jmp el ++
              label l ++
              pushq (ilab (nil_string ())) ++
              label el
          in
          n + 1, code
       | ce, _ -> n + 1, code ++ compile_expr ce ++ pushq !%rax
     in
     let n, code = List.fold_right f es (0, nop)  in
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

  | Ccall _ -> assert false

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
  let code ce ci1 ci2 =
    incr nb_if;
    let lab = "_" ^ string_of_int !nb_if in
    com ("start if" ^ lab) ++
      compile_expr ce ++
      testq (imm 1) !%rax ++
      je (".else" ^ lab) ++
      compile_instruction ci1 ++
      jmp (".endif" ^ lab) ++
      label (".else" ^ lab) ++
      compile_instruction ci2 ++
      label (".endif" ^ lab)
  in
  code

and jump_loop =
  let nb_loop = ref 0 in
  let code ce ci =
    incr nb_loop;
    let lab = "_" ^ string_of_int !nb_loop in
    com ("start loop" ^ lab) ++
      jmp (".end_loop" ^ lab) ++
      label (".loop" ^ lab) ++
      compile_instruction ci ++
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

and compile_instruction = function
  | Cnop -> nop
  | Cexpr e -> compile_expr e

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

  | Cdecl (ids, ce) ->
     let more = ref false in (* TODO : something better *)
     let code = com "declare" ++ compile_expr ce in
     List.fold_left
       (fun code id ->
         (if !more then code ++ popq rax else (more := true; code)) ++
           movq !%rax (ind ~ofs:id rbp))
       code ids

  | Cif (ce, ci1, ci2) -> jump_if ce ci1 ci2

  | Cfor (ce, ci) -> jump_loop ce ci

  | Cblock es ->
     List.fold_left (fun code i -> code ++ compile_instruction i)
       (com "compile new block") es

  | _ -> assert false

let build env =
  Smap.iter
    (fun name s ->
      let cs = compile_struct s in
      dbg "Compile structure `%s` (size %d).@." name cs.size;
      Hashtbl.add structs name cs) env.structs;
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

let gcc_command () =
  "gcc -g -no-pie %s -o %s" ^^
    (if !verbose then "" else " 1&>2 2>/dev/null")

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
    then Filename.temp_file "petitgo" ((Filename.basename !ofile) ^ ".s")
    else sprintf "%s.s" !ofile in

  dbg "Assembly code fully generated.@.";
  let f = open_out asm_file in
  let fmt = formatter_of_out_channel f in
  print_program fmt p;
  fprintf fmt "@?";
  close_out f;

  dbg "Compile assembly code with gcc.@.";
  let cmd = sprintf (gcc_command ()) asm_file !ofile in
  dbg "> %s@." cmd;

  let _ =
    try Sys.command cmd
    with Sys_error _ ->
      eprintf "An unexpected error occured while calling gcc.@.";
      eprintf "For more information try using -v option.@.";
      exit 2
  in

  if !keep_asm
  then dbg "Option -S is up so keep assembly code in %s@." asm_file
  else begin dbg "Detele assembly file.@."; Sys.remove asm_file; end
