open Parser
open Printf

(** [token_to_string token] returns a string representation of the token [token]. *)
let token_to_string = function
  | PACKAGE -> "PACKAGE"
  | IMPORT -> "IMPORT"
  | FUNC -> "FUNC"
  | VAR -> "VAR"
  | CONST -> "CONST"
  | FOR -> "FOR"
  | IF -> "IF"
  | ELSE -> "ELSE"
  | RETURN -> "RETURN"
  | INT -> "INT"
  | FLOAT -> "FLOAT"
  | COMPLEX -> "COMPLEX"
  | BOOL -> "BOOL"
  | STRING -> "STRING"
  | ASSIGN -> "ASSIGN"
  | DEFINE -> "DEFINE"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | LESST -> "LESST"
  | GREAT -> "GREAT"
  | EQUAL -> "EQUAL"
  | NOT_EQUAL -> "NOT_EQUAL"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | COMMA -> "COMMA"
  | SEMICOLON -> "SEMICOLON"
  | PRINTLN -> "PRINTLN"
  | INT_LIT i -> sprintf "INT_LIT '%Li'" i
  | FLOAT_LIT f -> sprintf "FLOAT_LIT '%f'" f
  | IMAG_LIT im -> sprintf "IMAG_LIT '%fi'" im
  | BOOL_LIT b -> sprintf "BOOL_LIT '%b'" b
  | STRING_LIT s -> sprintf "STRING_LIT '%s'" s
  | IDENT id -> sprintf "IDENT '%s'" id.content
  | EOF -> "EOF"

(** [print lexbuf] prints the tokens obtained from the lexical analysis of [lexbuf] on the standard output. *)
let print lexbuf =
  let rec get_tokens acc lexbuf =
    let token = Lexer.read_token lexbuf in
    match token with
    | EOF -> acc ^ "EOF"
    | _ ->
        let acc' = sprintf "%s%s\n" acc (token_to_string token) in
        get_tokens acc' lexbuf
  in
  printf "%s\n" (get_tokens "" lexbuf)
