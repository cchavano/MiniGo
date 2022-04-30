open Lexing
open Mini_go

let go_file = ref ""

let target_dir = "target"

let tgc_path = "tgc"

let usage_msg = "Usage: mini-go [options] <file>\noptions:"

let dump_tokens = ref false

let dump_ast = ref false

let options =
  [
    ("--dump-tokens", Arg.Set dump_tokens, "print all tokens (stop after lexing)");
    ("--dump-ast", Arg.Set dump_ast, "print the abstract syntax tree (stop after parsing)");
  ]

let set_file file = go_file := file

let show_usage options msg = Arg.usage options msg

let () =
  Arg.parse options set_file usage_msg;

  if !go_file = "" then begin
    Printf.fprintf stderr "Error: the Go file is missing\n";
    show_usage options usage_msg;
    exit 1
  end;

  let input = open_in !go_file in

  let lexbuf = from_channel input in

  lexbuf.lex_curr_p <- { pos_fname = !go_file; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 };

  try
    if !dump_tokens then begin
      Print_tokens.print lexbuf;
      close_in input;
      exit 0
    end;

    let program = Parser.program Lexer.read_token lexbuf in

    if !dump_ast then begin
      Print_ast.print program;
      close_in input;
      exit 0
    end;

    let checked_program = Checker.check_program program in

    let raw_filename = !go_file |> Filename.basename |> Filename.remove_extension in

    let c_file = Printf.sprintf "%s%s%s.c" target_dir "/C/" raw_filename in

    let exe_file = Printf.sprintf "%s%s%s" target_dir "/exe/" raw_filename in

    let output = open_out c_file in

    Go2c.program_2c output checked_program;

    close_out output;

    let compile_cmd =
      Printf.sprintf "cc %s -o %s -I%s %s/tgc.o" c_file exe_file tgc_path tgc_path
    in

    match Unix.system compile_cmd with
    | Unix.WEXITED code ->
        Printf.fprintf
          stdout
          "-> C file generated at \"%s\".\n-> Exe file generated at \"%s\".\n"
          c_file
          exe_file;
        exit code
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
