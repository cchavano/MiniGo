open Lexing
open Mini_go

let file = "test/examples/factorial.go"

let () =
  let input = open_in file in
  let lexbuf = from_channel input in

  lexbuf.lex_curr_p <- {
    pos_fname = file;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0
  };

  try
    Print_tokens.print_tokens lexbuf;
    close_in input
  with Lexer.Error msg ->
        Printf.fprintf
          stderr
          "%s %s\n"
          (Pretty_error.from_single_pos (Lexing.lexeme_start_p lexbuf))
          msg