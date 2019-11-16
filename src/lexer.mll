{
    open Parser

    exception Lexing_error of string
    let lexing_error msg = raise (Lexing_error msg)

    (* true if insert semicolon on new line *)
    let is_semi = ref false

    let eol f lexbuf =
      Lexing.new_line lexbuf;
      if !is_semi
      then begin is_semi := false; SEMI end
      else f lexbuf

    let tok t =
      let _ =
        match t with
        | IDENT _ | INT _ | STRING _ | TRUE | FALSE
          | NIL | RETURN | INCR | DECR | RPAR | END ->
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

rule token = parse
| blank         { token lexbuf }
| new_line      { eol token lexbuf }
| "/*"          { comment lexbuf }
| "//"          { comment_line lexbuf }
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
| '"'           { string (Buffer.create 256) lexbuf }
| '-' blank* "9223372036854775808"
| '-' blank* "0x8000000000000000"   { INT Int64.min_int }
| integer as i  { try INT (Int64.of_string i) |> tok
                  with Failure _ ->
                    lexing_error (i ^ " is too big for 64-bit integer") }
| ident   as id { let t =
                    try Hashtbl.find keywords id
                    with Not_found -> IDENT id
                  in tok t }
| eof           { tok EOF }
| _ as c        { lexing_error ("Unexpected " ^ (Char.escaped c)) }

and string buf = parse
| '"'                { STRING (Buffer.contents buf) |> tok }
| '\\' '\\'          { Buffer.add_char buf '\\'; string buf lexbuf }
| '\\' 'n'           { Buffer.add_char buf '\n'; string buf lexbuf }
| '\\' 't'           { Buffer.add_char buf '\r'; string buf lexbuf }
| '\\' '"'           { Buffer.add_char buf '"' ; string buf lexbuf }
| [^ '"' '\\']+ as s { Buffer.add_string buf s; string buf lexbuf }
| _ as c { lexing_error ("Illegal string character " ^ (Char.escaped c) ^ ".") }
| eof    { lexing_error "Unclosed string." }

and comment = parse
| "*/"     { token lexbuf }
| new_line { eol comment lexbuf }
| _        { comment lexbuf }
| eof      { lexing_error "Unclosed comment" }

and comment_line = parse
| new_line { eol token lexbuf }
| _        { comment_line lexbuf }
| eof      { EOF }
