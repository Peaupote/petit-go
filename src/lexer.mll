{
    open Parser

    exception Lexing_error of string
    let lexing_error msg = raise (Lexing_error msg)

    (* true of insert semicolon on new line *)
    let is_semi = ref false

    let tok t =
      let _ =
        match t with
        | IDENT _ | INT _ | STRING _ | TRUE | FALSE
          | NIL | RETURN | INCR | DECR | LPAR | END ->
           is_semi := true
        | _       -> is_semi := false
      in
      t

    let keywords = Hashtbl.create 100
    let _ = List.iter (fun (w, t) -> Hashtbl.add keywords w t)
          ["else", ELSE;
           "import", IMPORT;
           "true", TRUE;
           "false", FALSE;
           "nil", NIL;
           "type", TYPE;
           "for", FOR;
           "package", PACKAGE;
           "var", VAR;
           "func", FUNC;
           "return", RETURN;
           "if", IF;
           "struct", STRUCT]
}

let new_line    = '\n' | '\r' | "\n\r"
let blank       = [' ' '\t']+
let digit       = ['0'-'9']
let alpha       = ['a'-'z' 'A'-'Z' '_']
let ident       = alpha(alpha|digit)*
let decimal_int = digit+
let hex_int     = ("0x"|"0X")['0'-'9' 'a'-'f' 'A'-'F']+
let integer     = decimal_int | hex_int
let char        = [^ '"' '\\' '\n' '\t']
let string      = '"' char* '"'

rule token = parse
| blank         { token lexbuf }
| new_line      { Lexing.new_line lexbuf;
                  if !is_semi
                  then begin is_semi := false; SEMI end
                  else token lexbuf }
| "/*"          { is_semi := false; comment lexbuf }
| "//"          { is_semi := false; comment_line lexbuf }
| "{"           { tok BEGIN }
| "}"           { tok END }
| "("           { tok LPAR }
| ")"           { tok RPAR }
| ";"           { tok SEMI }
| ","           { tok COMMA }
| "."           { tok DOT }
| "&"           { tok AMP }
| "!"           { tok NOT }
| "="           { tok EQ }
| ":="          { tok CEQ }
| "=="          { tok BEQ }
| "!="          { tok NEQ }
| "<"           { tok LT }
| ">"           { tok GT }
| "<="          { tok LEQ }
| ">="          { tok GEQ }
| "+"           { tok PLUS }
| "-"           { tok MINUS }
| "*"           { tok STAR }
| "/"           { tok DIV }
| "%"           { tok MOD }
| "&&"          { tok AND }
| "||"          { tok OR }
| "++"          { tok INCR }
| "--"          { tok DECR }
| "*"           { tok STAR }
| string  as s  { STRING s |> tok }
| integer as i  { INT (int_of_string i) |> tok }
| ident   as id { let t =
                    try Hashtbl.find keywords id
                    with Not_found -> IDENT id
                  in tok t }
| eof           { tok EOF }
| _ as c        { lexing_error ("Unexpected " ^ (Char.escaped c)) }

and comment = parse
| "*/"     { token lexbuf }
| new_line { Lexing.new_line lexbuf; comment lexbuf }
| _        { comment lexbuf }
| eof      { lexing_error "Unclosed comment." }

and comment_line = parse
| new_line { Lexing.new_line lexbuf; token lexbuf }
| _        { comment_line lexbuf }
| eof      { EOF }
