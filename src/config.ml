open Format

let parse_only = ref false
let type_only  = ref false
let verbose    = ref false

let ifile = ref ""
let ofile = ref ""

let options = [
    "--parse-only", Arg.Set parse_only, " Stop execution after parsing";
    "--type-only",  Arg.Set type_only,  " Stop execution after typing";
    "-v",           Arg.Set verbose,    " Verbose mode"
  ]

let usage = "usage: " ^ Sys.argv.(0) ^ " file"

let dbg f =
  let ppf = if !verbose then std_formatter else
              make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
  fprintf ppf f
