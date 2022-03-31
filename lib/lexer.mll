{  
  open Lexing
  open Printf

  type token =
    | PACKAGE
    | IMPORT
    | FUNC
    | VAR | CONST
    | FOR
    | IF | ELSE
    | RETURN
    | INT | FLOAT | COMPLEX | BOOL | STRING
    | ASSIGN | DCL_ASSIGN
    | PLUS | MINUS | MULT | DIV
    | LESST | GREAT | EQUAL | NOT_EQUAL
    | AND | OR
    | NOT
    | LPAREN| RPAREN | LBRACE | RBRACE
    | COLON | COMMA | SEMICOLON
    | PRINTLN
    | INT_LIT of int64
    | FLOAT_LIT of float
    | IMAG_LIT of float
    | BOOL_LIT of bool
    | STRING_LIT of string
    | IDENT of string
    | EOF

  exception Error of string

  let keywords = Hashtbl.create 14

  let () =
    List.iter
      (fun (s, t) -> Hashtbl.add keywords s t) 
      [
        "package", PACKAGE; "import", IMPORT; "func", FUNC; "var", VAR; "const", CONST;
        "for", FOR; "if", IF; "else", ELSE; "return", RETURN; "int", INT; "float64", FLOAT;
        "complex128", COMPLEX; "bool", BOOL; "string", STRING
      ]

  let error msg = raise @@ Error msg

  let prev_token = ref (Option.none)

  let tok token =
    prev_token := Option.some token;
    token
}

let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let space = [' ''\t''\r']

let int_lit = (digit (digit | '_')*)* digit
let float_lit = (int_lit '.' int_lit?) | (int_lit? '.' int_lit)
let imag_lit = (int_lit | float_lit) 'i'
let bool_lit = "true" | "false"
let string_lit = '"' (letter | digit | space | '_')* '"'

let ident = letter (letter | digit | '_')*

rule read_token = parse
  | "//" [^'\n']* '\n'
  | '\n'
    { 
      let go_forward lexbuf = new_line lexbuf; read_token lexbuf in
      if Option.is_some !prev_token then
        begin
          let value = Option.get !prev_token in
          match value with
          | IDENT _
          | INT | FLOAT | COMPLEX | BOOL | STRING
          | RPAREN | RBRACE
          | RETURN -> new_line lexbuf; tok SEMICOLON
          | _ -> go_forward lexbuf
        end
      else go_forward lexbuf
    }
  | space+  { read_token lexbuf }
  | "/*"    { read_comment lexbuf }
  | '('     { tok LPAREN }
  | ')'     { tok RPAREN }
  | '{'     { tok LBRACE }
  | '}'     { tok RBRACE }
  | ','     { tok COMMA }
  | ':'     { tok COLON }
  | ';'     { tok SEMICOLON }
  | ":="    { tok DCL_ASSIGN }
  | "="     { tok ASSIGN }
  | "+"     { tok PLUS }
  | "-"     { tok MINUS }
  | "*"     { tok MULT }
  | "/"     { tok DIV }
  | "<"     { tok LESST }
  | ">"     { tok GREAT }
  | "=="    { tok EQUAL }
  | "!="    { tok NOT_EQUAL }
  | "!"     { tok NOT }
  | int_lit as i
    {
      try
        tok @@ INT_LIT (Int64.of_string i)
      with Failure _ ->
        error (sprintf "int literal '%s' overflows the maximum value of int64" i)   
    }
  | float_lit as f
    {
      try
        tok @@ FLOAT_LIT (float_of_string f)
      with Failure _ ->
        error (sprintf "invalid float literal '%s'" f)
    }
  | imag_lit as im
    {
      try
        let imag_part = String.sub im 0 (String.length im - 1) in
        tok @@ IMAG_LIT (float_of_string imag_part)
      with Failure _ ->
          error (sprintf "invalid imaginary literal '%s'" im)
    }
  | bool_lit as b   { tok @@ BOOL_LIT (bool_of_string b) }
  | string_lit as s { tok @@ STRING_LIT s }
  | ident as id
    {
      try
        tok (Hashtbl.find keywords id)
      with Not_found -> tok (IDENT id)
    }
  | "fmt.Println"   { tok PRINTLN }
  | eof             { tok EOF }
  | _ as c 
    {
      error (Printf.sprintf "illegal character '%s'" (String.make 1 c))
    }

and read_comment = parse
  | "*/"  { read_token lexbuf }
  | '\n'  { new_line lexbuf; read_comment lexbuf }
  | eof   { error "unterminated comment" }
  | _     { read_comment lexbuf }