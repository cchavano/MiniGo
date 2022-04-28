open Lexing
open Mini_go

let ifile = "test/examples/factorial.go"

let target_dir = "target"

let tgc_path = "tgc"

let ofile = target_dir ^ "/c/factorial.c"

let efile = target_dir ^ "/exe/factorial"

let () =
  let input = open_in ifile in
  let lexbuf = from_channel input in

  lexbuf.lex_curr_p <-
    { pos_fname = ifile; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };

  try
    (* Print_tokens.print_tokens lexbuf; *)
    let prog = Parser.program Lexer.read_token lexbuf in
    (* Print_ast.print_ast prog; *)
    let checked_prog = Checker.check_program prog in
    let output = open_out ofile in
    Go2c.program_2c output checked_prog;
    close_in input;
    close_out output;
    let compile =
      Printf.sprintf "gcc -o %s %s -I%s %s/tgc.o" efile ofile tgc_path tgc_path
    in
    match Unix.system compile with
    | Unix.WEXITED code -> exit code
    | _ -> exit 1
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
