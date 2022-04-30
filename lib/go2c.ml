open Ast_typ
open Printf

let indent = String.make 4 ' '

module StringSet = Set.Make (String)

let option_to_string empty f = function
  | None -> empty
  | Some e -> f e

let rec list_to_string sep f = function
  | [] -> ""
  | [x] -> f x
  | x :: r -> sprintf "%s%s%s" (f x) sep (list_to_string sep f r)

let unop_2c = function
  | UOpNot -> "!"
  | UOpPlus -> "+"
  | UOpMinus -> "-"

let arthop_2c = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"

let comparop_2c = function
  | OpLesst -> "<"
  | OpGreat -> ">"
  | OpEqual -> "=="
  | OpNotEqual -> "!="

let logicop_2c = function
  | OpAnd -> "&&"
  | OpOr -> "||"

let binop_2c = function
  | OpArithmetic op -> arthop_2c op
  | OpCompare op -> comparop_2c op
  | OpLogic op -> logicop_2c op

let literal_2c = function
  | LitInt i -> Int64.to_string i
  | LitFloat f -> Float.to_string f
  | LitImag im -> Float.to_string im ^ "i"
  | LitBool b -> Bool.to_string b
  | LitString s -> s

let value_2c = function
  | ValInt i -> Int64.to_string i
  | ValFloat f -> Float.to_string f
  | ValComplex c -> sprintf "(%f + %f*I)" c.re c.im
  | ValBool b -> Bool.to_string b
  | ValString s -> s

let basic_typ_2c = function
  | TypInt -> "int"
  | TypFloat -> "double"
  | TypComplex -> "double complex"
  | TypBool -> "bool"
  | TypString -> "string"

let rec typ_2c = function
  | TypBasic b -> basic_typ_2c b
  | TypFunc (l, t) -> sprintf "%s (*) (%s)" (typ_option_2c t) (typ_list_2c l)

and typ_option_2c typ = option_to_string "void" basic_typ_2c typ

and typ_list_2c tl = list_to_string ", " typ_2c tl

let rec expression_2c venv fenv e =
  if e.mode = ModUntyped || e.mode = ModConstant then value_2c (Option.get e.value)
  else
    match e.raw with
    | ELiteral lit -> literal_2c lit
    | EIdentRef id ->
        let prefix = if StringSet.mem id venv then "_" else "def_" in
        prefix ^ id
    | EFuncCall (id, args) ->
        let sid =
          if StringSet.mem id venv then "_" ^ id
          else if StringSet.mem id fenv then if id = "main" then id else "def_" ^ id
          else
            sprintf
              "%s"
              (match id with
              | "len" -> "strlen"
              | "real" -> "creal"
              | "imag" -> "cimag"
              | "complex" -> "complex_of"
              | _ -> assert false)
        in
        sprintf "%s(%s)" sid (expression_list_2c venv fenv args)
    | EUnOp (uop, e) -> sprintf "%s%s" (unop_2c uop) (expression_2c venv fenv e)
    | EBinOp (op, e1, e2) -> binop_expression_2c venv fenv op e1 e2
    | EConversion (typ, e) -> sprintf "((%s)%s)" (typ_2c typ) (expression_2c venv fenv e)

and binop_expression_2c venv fenv op e1 e2 =
  match (e2.typ, e2.typ) with
  | TypBasic b1, TypBasic b2 -> begin
      match (op, b1, b2) with
      | OpArithmetic OpAdd, TypString, TypString ->
          sprintf
            "string_concat(%s, %s)"
            (expression_2c venv fenv e1)
            (expression_2c venv fenv e2)
      | _ ->
          sprintf
            "%s %s %s"
            (expression_2c venv fenv e1)
            (binop_2c op)
            (expression_2c venv fenv e2)
    end
  | _ -> assert false

and expression_list_2c venv fenv el = list_to_string ", " (expression_2c venv fenv) el

let rec statement_2c prefix venv fenv s =
  let prefix' = prefix ^ indent in
  match s with
  | StVarDecl (id, typ, e) ->
      let sdecl =
        match typ with
        | TypBasic b1 -> sprintf "%s _%s" (basic_typ_2c b1) id
        | TypFunc (l, t) ->
            sprintf "%s%s (*_%s) (%s)" prefix (typ_option_2c t) id (typ_list_2c l)
      in
      let sinit = expression_init_2c venv fenv typ e in
      (sprintf "%s%s = %s;" prefix' sdecl sinit, StringSet.add id venv)
  | StConstDecl _ -> ("", venv)
  | StIfElse (e, s1, s2) ->
      let s1', _ = statement_2c prefix' venv fenv s1 in
      let s2', _ = statement_2c prefix' venv fenv s2 in
      let sif =
        sprintf "%sif (%s) %s else %s" prefix' (expression_2c venv fenv e) s1' s2'
      in
      (sif, venv)
  | StWhileFor (e, s) ->
      let s', _ = statement_2c prefix' venv fenv s in
      let swhile = sprintf "%swhile (%s) %s" prefix' (expression_2c venv fenv e) s' in
      (swhile, venv)
  | StAssign (id, e) ->
      let sassign =
        match e.typ with
        | TypBasic TypString ->
            sprintf
              "%sstring_assign(%s, %s);"
              prefix'
              ("_" ^ id)
              (expression_2c venv fenv e)
        | _ -> sprintf "%s_%s = %s;" prefix' id (expression_2c venv fenv e)
      in
      (sassign, venv)
  | StBlock sl ->
      let sl', _ = statement_list_2c prefix venv fenv sl in
      let sblock = sprintf "{\n%s\n%s}" sl' prefix in
      (sblock, venv)
  | StPrintln args ->
      let fmt =
        list_to_string
          " "
          (fun arg ->
            match arg.typ with
            | TypBasic b -> begin
                match b with
                | TypInt -> "%d"
                | TypFloat -> "%.17g"
                | TypComplex -> "%s"
                | TypBool -> "%s"
                | TypString -> "%s"
              end
            | TypFunc _ -> "%p")
          args
      in
      let args' =
        list_to_string
          ", "
          (fun arg ->
            let arg_as_2c = expression_2c venv fenv arg in
            match arg.typ with
            | TypBasic b -> begin
                match b with
                | TypBool -> sprintf "bool_to_string(%s)" arg_as_2c
                | TypComplex -> sprintf "complex_to_string(%s)" arg_as_2c
                | _ -> arg_as_2c
              end
            | TypFunc _ -> arg_as_2c)
          args
      in
      let sprint = sprintf "%sprintf(\"%s\\n\", %s);" prefix' fmt args' in
      (sprint, venv)

and expression_init_2c venv fenv typ e =
  match e with
  | None -> begin
      match typ with
      | TypBasic b -> (
          match b with
          | TypInt -> "0"
          | TypFloat -> "0."
          | TypComplex -> "0."
          | TypBool -> "false"
          | TypString -> "string_init(\"\")")
      | TypFunc _ -> "NULL"
    end
  | Some v -> (
      let e' = expression_2c venv fenv v in
      match typ with
      | TypBasic TypString -> sprintf "string_init(%s)" e'
      | _ -> e')

and statement_list_2c prefix venv fenv sl =
  let get_separator = function
    | StConstDecl _ -> ""
    | _ -> "\n"
  in
  let rec get_statements (stl, venv) fenv = function
    | [] -> (stl, venv)
    | [x] ->
        let x', venv' = statement_2c prefix venv fenv x in
        (stl ^ x', venv')
    | x :: r ->
        let x', venv' = statement_2c prefix venv fenv x in
        let stl' = sprintf "%s%s%s" stl x' (get_separator x) in
        get_statements (stl', venv') fenv r
  in
  get_statements ("", venv) fenv sl

let func_2c fenv func =
  let func_param_2c param =
    match snd param with
    | TypBasic _ -> sprintf "%s _%s" (typ_2c @@ snd param) (fst param)
    | TypFunc (l, t) ->
        sprintf "%s (*_%s) (%s)" (typ_option_2c t) (fst param) (typ_list_2c l)
  in
  let venv = List.map (fun p -> fst p) func.params |> StringSet.of_list in
  let sl, venv' = statement_list_2c "" venv fenv func.body in
  sprintf
    "%s %s%s(%s) {\n%s%s%sreturn %s;\n}\n"
    (typ_option_2c func.result)
    "def_"
    func.name
    (list_to_string ", " func_param_2c func.params)
    sl
    (if List.length func.body > 0 then "\n" else "")
    indent
    (option_to_string "" (expression_2c venv' fenv) func.return)

let main_2c fenv func =
  sprintf
    "int main(int argc, char **argv) {\n\
     %stgc_start(&gc, &argc);\n\
     %s\n\
     %stgc_stop(&gc);\n\
     %sreturn 0;\n\
     }"
    indent
    (fst @@ statement_list_2c "" StringSet.empty fenv func.body)
    indent
    indent

let cfunc_bool_to_string =
  sprintf
    "__attribute__((noinline)) static char* bool_to_string(bool b) {\n\
     %sint len = (b ? 5 : 6) + 1;\n\
     %schar *res = tgc_alloc(&gc, len);\n\
     %sb ? strcpy(res, \"true\") : strcpy(res, \"false\");\n\
     %sreturn res;\n\
     }\n"
    indent
    indent
    indent
    indent

let cfunc_complex_to_string =
  sprintf
    "__attribute__((noinline)) static char* complex_to_string(double complex c) {\n\
     %schar* res = tgc_alloc(&gc, 100);\n\
     %ssprintf(res, \"(%%.17g+%%.17gi)\", creal(c), cimag(c));\n\
     %sreturn res;\n\
     }\n"
    indent
    indent
    indent

let cfunc_string_init =
  sprintf
    "__attribute__((noinline)) static char* string_init(const char *s) {\n\
     %schar *res = tgc_alloc(&gc, strlen(s) + 1);\n\
     %sstrcpy(res, s);\n\
     %sreturn res;\n\
     }\n"
    indent
    indent
    indent

let cfunc_string_assign =
  sprintf
    "__attribute__((noinline)) static void string_assign(char *dest, const char *src) {\n\
     %stgc_realloc(&gc, (void *)dest, strlen(src) + 1);\n\
     %sstrcpy(dest, src);\n\
     }\n"
    indent
    indent

let cfunc_string_concat =
  sprintf
    "__attribute__((noinline)) static char* string_concat(const char *s1, const char \
     *s2) {\n\
     %ssize_t len1 = strlen(s1);\n\
     %schar *res = tgc_alloc(&gc, len1 + strlen(s2) + 1);\n\
     %sstrcpy(res, s1);\n\
     %sstrcpy(res + len1, s2);\n\
     %sreturn res;\n\
     }\n"
    indent
    indent
    indent
    indent
    indent

let cfunc_complex_of =
  sprintf
    "static inline double complex complex_of(double re, double im) {\n\
     %sreturn re + im * I;\n\
     }\n"
    indent

let program_2c out program =
  let defs_wo_main = List.filter (fun f -> f.name <> "main") program.defs in
  let def_main = List.find (fun f -> f.name = "main") program.defs in
  let fenv = List.map (fun f -> f.name) defs_wo_main |> List.to_seq |> StringSet.of_seq in
  fprintf
    out
    "#include <stdlib.h>\n\
     #include <stdio.h>\n\
     #include <string.h>\n\
     #include <stdbool.h>\n\
     #include <complex.h>\n\
     #include \"tgc.h\"\n\n\
     typedef char *string;\n\n\
     static tgc_t gc;\n\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s\n\
     %s"
    cfunc_string_init
    cfunc_string_assign
    cfunc_string_concat
    cfunc_bool_to_string
    cfunc_complex_to_string
    cfunc_complex_of
    (list_to_string "\n\n" (func_2c fenv) defs_wo_main)
    (main_2c fenv def_main)
