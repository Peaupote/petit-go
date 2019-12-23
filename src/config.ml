open Format

let parse_only = ref false
let type_only  = ref false
let verbose    = ref false
let exec       = ref false
let allow_unused_var = ref false
let allow_unused_package = ref false
let quiet_mode = ref false

let ifile = ref ""
let ofile = ref ""

let options = [
    "--parse-only", Arg.Set parse_only,   " Stop execution after parsing";
    "--type-only",  Arg.Set type_only,    " Stop execution after typing";
    "-v",           Arg.Set verbose,      " Verbose mode";
    "-o",           Arg.Set_string ofile, "<file> Name of compiled file";
    "-E",           Arg.Set exec,         " Produce executable code";
    "--allow-unused", Arg.Tuple [Arg.Set allow_unused_package;
                                 Arg.Set allow_unused_var],
                      " Allows unused packages, vars...";
    "--allow-unused-var", Arg.Set allow_unused_var,
                          " Allows unused vars in code";
    "--allow-unused-pkg", Arg.Set allow_unused_package,
                          " Allow unused packafes";
    "--quiet", Arg.Set quiet_mode, " Disable warnings"
  ]

let usage = "usage: " ^ Sys.argv.(0) ^ " file"

let dbg f =
  let ppf = if !verbose then std_formatter else
              make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
  fprintf ppf f

let warn f =
  let ppf =
    if !quiet_mode
    then make_formatter (fun _ _ _ -> ()) (fun _ -> ())
    else make_formatter
           (fun s _ _ -> output_string stdout s)
           (fun () -> flush stdout)
  in
  fprintf ppf "Warning: "; fprintf ppf f

module Smap = Map.Make(String)
module Vset = Set.Make(String)
module Iset = Set.Make(struct type t = int let compare = compare end)
