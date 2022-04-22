open Lexing
open Mini_go

let file = "test/examples/factorial.go"

let () =
  let input = open_in file in
  let lexbuf = from_channel input in

  lexbuf.lex_curr_p <-
    { pos_fname = file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };

  try
    (* Print_tokens.print_tokens lexbuf; *)
    let prog = Parser.program Lexer.read_token lexbuf in
    (* Print_ast.print_ast prog; *)
    let _ = Checker.check_program prog in
    close_in input
  with
  | Lexer.Error msg ->
      Printf.fprintf
        stderr
        "%s %s\n"
        (Pretty_error.from_single_pos (Lexing.lexeme_start_p lexbuf))
        msg
  | Parser.Error ->
      Printf.fprintf
        stderr
        "%s syntax error\n"
        (Pretty_error.from_single_pos (Lexing.lexeme_start_p lexbuf))
  | Checker.Error msg -> Printf.fprintf stderr "%s\n" msg
