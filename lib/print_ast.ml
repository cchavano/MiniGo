open Printf
open Ast_loc
open Location

let branch = "\xe2\x94\x9c"

let branch_end = "\xe2\x94\x94"

let pipe = "\xe2\x94\x82"

let indentation = String.make 2 ' '

let unop_to_string = function
  | UOpNot -> "!"
  | UOpPlus -> "+"
  | UOpMinus -> "-"

let arthop_to_string = function
  | OpPlus -> "+"
  | OpMinus -> "-"
  | OpMult -> "*"
  | OpDiv -> "/"

let comparop_to_string = function
  | OpLesst -> "<"
  | OpGreat -> ">"
  | OpEqual -> "=="
  | OpNotEqual -> "!="

let logicop_to_string = function
  | OpAnd -> "&&"
  | OpOr -> "||"

let binop_to_string = function
  | OpArithmetic op -> arthop_to_string op
  | OpCompare op -> comparop_to_string op
  | OpLogic op -> logicop_to_string op

let literal_to_string = function
  | LitInt i -> sprintf "LitInt %Li" i
  | LitFloat d -> sprintf "LitDouble %f" d
  | LitImag im -> sprintf "LitImag %fi" im
  | LitBool b -> sprintf "LitBool %b" b
  | LitString s -> sprintf "LitString %s" s

let basic_typ_to_string = function
  | TypInt -> "int"
  | TypFloat -> "float64"
  | TypComplex -> "complex128"
  | TypBool -> "bool"
  | TypString -> "string"

let rec typ_to_string = function
  | TypBasic b -> basic_typ_to_string b
  | TypFunc (tl, t) -> 
      sprintf "func(%s) %s" (typ_list_to_string tl) (basic_typ_to_string t)

and typ_list_to_string = function
  | [] -> ""
  | [x] -> sprintf "%s" (typ_to_string x)
  | x :: r -> sprintf "%s %s" (typ_to_string x) (typ_list_to_string r)

let option_to_string empty f = function
  | None -> empty
  | Some e -> f e

let list_to_string f prefix l =
  let prefix' = prefix ^ indentation in
  let rec lts = function
    | [] -> ""
    | [x] ->
        sprintf "%s%s%s"
          prefix'
          branch_end
          (f (prefix' ^ " ") x)
    | x :: r ->
        sprintf "%s%s%s\n%s"
          prefix'
          branch
          (f (prefix' ^ pipe) x)
          (lts r)
  in lts l

let rec expression_to_string prefix loc =
  raw_expression_to_string prefix loc.content

and expression_list_to_string prefix l =
  list_to_string expression_to_string prefix l

and raw_expression_to_string prefix e =
  let prefix' = prefix ^ indentation in
  match e with
  | ELiteral lit -> sprintf "ELiteral (%s)" (literal_to_string lit)
  | EIdentRef id -> sprintf "EIdentRef '%s'" id.content
  | EFuncCall (id, args) ->
      sprintf "EFuncCall '%s'\n%s%s()\n%s"
        id.content
        prefix'
        branch_end
        (expression_list_to_string prefix' args)
  | EValueCast(t, e) ->
      sprintf "EValueCast %s\n%s%s%s"
        (basic_typ_to_string t)
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | EUnOp (uop, e) ->
      sprintf "EUnOp '%s'\n%s%s%s"
        (unop_to_string uop)
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | EBinOp (op, e1, e2) ->
      sprintf "EBinop '%s'\n%s%s%s\n%s%s%s"
        (binop_to_string op)
        prefix'
        branch
        (expression_to_string (prefix' ^ pipe) e1)
        prefix'
        branch_end
        (expression_to_string prefix' e2)

and statement_list_to_string prefix l =
    list_to_string statement_to_string prefix l

and statement_to_string prefix s =
  let prefix' = prefix ^ indentation in
  match s with
  | StVarDecl (id, t, e) ->
      let expr =
        match e with
        | None -> ""
        | Some v ->
            sprintf "\n%s%s%s"
              prefix'
              branch_end
              (expression_to_string prefix' v)
      in
      sprintf "StVarDecl '%s' %s%s"
        id.content
        (option_to_string "" typ_to_string t)
        expr
  | StConstDecl (id, t, e) ->
      sprintf "StConstDecl '%s' %s\n%s%s%s"
        id.content
        (option_to_string "untyped" typ_to_string t)
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | StIfElse (e, s1, s2) ->
      sprintf "StIfElse\n%s%s%s\n%s%s%s\n%s%s%s"
        prefix'
        branch
        (expression_to_string (prefix' ^ pipe) e)
        prefix'
        branch
        (statement_to_string (prefix' ^ pipe) s1)
        prefix'
        branch_end
        (statement_to_string prefix' s2)
  | StWhileFor (e, s) ->
      sprintf "StWhileFor\n%s%s%s\n%s%s%s"
        prefix'
        branch
        (expression_to_string (prefix' ^ pipe) e)
        prefix'
        branch_end
        (statement_to_string prefix' s)
  | StAssign (id, e) ->
      sprintf "StAssign\n%s%s%s\n%s%s%s"
        prefix'
        branch
        id.content
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | StBlock l ->
      sprintf "StBlock\n%s"
        (statement_list_to_string prefix l)
  | StPrintln args ->
      sprintf "StPrintln\n%s%s()\n%s"
        prefix'
        branch_end
        (expression_list_to_string prefix' args)

and func_list_to_string prefix l =
  list_to_string func_to_string prefix l

and param_list_to_string = function
  | [] -> ""
  | [x] ->
      sprintf "%s %s"
        (fst x).content
        (typ_to_string @@ snd x)
  | x :: r ->
      sprintf "%s %s, %s"
        (fst x).content
        (typ_to_string @@ snd x)
        (param_list_to_string r)

and func_to_string prefix func =
  let prefix' = prefix ^ indentation in
  let returned_expr =
    match func.return with 
    | None -> ""
    | Some v ->
        sprintf "\n%s%s%s" 
        (prefix' ^ indentation)
        branch_end
        (expression_to_string (prefix' ^ indentation ) v)
  in
  sprintf "FuncDecl '%s'\n%s%sparams (%s)\n%s%sbody%s%s\n%s%sresult %s\n%s%sreturn%s"
    func.name.content
    prefix'
    branch
    (param_list_to_string func.params)
    prefix'
    branch
    (if func.body = [] then "" else "\n")
    (statement_list_to_string (prefix' ^ pipe) func.body)
    prefix'
    branch
    (option_to_string "" basic_typ_to_string func.result)
    prefix'
    branch_end
    returned_expr

let print_ast prog = 
  printf "program\n%s\n" @@ 
    func_list_to_string "" (prog.defs)