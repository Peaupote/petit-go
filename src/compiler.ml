open Format
open Config
open X86_64
open Ast

let genv : (string, unit) Hashtbl.t = Hashtbl.create 10

let compile env = ignore(env); nop, nop

let gcc_command = "gcc -g -no-pie %s" ^^ ""

let compile_program compile_order =
  let code_main, code_funcs =
    Queue.fold
      (fun (code_main, code_funcs) pkg ->
        dbg "Start compiling %s@." pkg;
        let cmain, cfuncs = compile (Smap.find pkg !all_packages) in
        code_main ++ cmain, code_funcs ++ cfuncs )
      (nop, nop) compile_order
  in

  let p =
    { text =
        globl "main" ++ label "main" ++
          movq !%rsp !%rbp ++
          code_main ++
          movq (imm 0) !%rax ++ (* exit *)
          ret ++
          label "print_int" ++
          movq !%rdi !%rsi ++
          movq (ilab ".Sprint_int") !%rdi ++
          movq (imm 0) !%rax ++
          call "printf" ++
          ret ++
          code_funcs;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in

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
  let cmd = sprintf gcc_command asm_file in
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
