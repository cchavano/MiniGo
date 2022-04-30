open Printf
open Ast_loc
open Location

let branch = "\xe2\x94\x9c"

let branch_end = "\xe2\x94\x94"

let pipe = "\xe2\x94\x82"

let indent = String.make 2 ' '

(** [option_to_string snone fsome o] returns a string given by [fsome] applied to [v] if [o] is [Some v], else returns [snone]. *)
let option_to_string snone fsome = function
  | None -> snone
  | Some v -> fsome v

(** [unop_to_string op] returns a string representation of the unary operator [op]. *)
let unop_to_string = function
  | UOpNot -> "!"
  | UOpPlus -> "+"
  | UOpMinus -> "-"

(** [arthop_to_string op] returns a string representation of the arithmetic operator [op]. *)
let arthop_to_string = function
  | OpAdd -> "+"
  | OpSub -> "-"
  | OpMul -> "*"
  | OpDiv -> "/"

(** [comparop_to_string op] returns a string representation of the comparison operator [op]. *)
let comparop_to_string = function
  | OpLesst -> "<"
  | OpGreat -> ">"
  | OpEqual -> "=="
  | OpNotEqual -> "!="

(** [logicop_to_string op] returns a string representation of the logical operator [op]. *)
let logicop_to_string = function
  | OpAnd -> "&&"
  | OpOr -> "||"

(** [binop_to_string op] returns a string representation of the binary operator [op]. *)
let binop_to_string = function
  | OpArithmetic op -> arthop_to_string op
  | OpCompare op -> comparop_to_string op
  | OpLogic op -> logicop_to_string op

(** [literal_to_string lit] returns a string representation of the literal [lit]. *)
let literal_to_string = function
  | LitInt i -> sprintf "LitInt %Li" i
  | LitFloat f -> sprintf "LitFloat %f" f
  | LitImag im -> sprintf "LitImag %fi" im
  | LitBool b -> sprintf "LitBool %b" b
  | LitString s -> sprintf "LitString %s" s

(** [basic_typ_to_string typ] returns a string representation of the basic type [typ]. *)
let basic_typ_to_string = function
  | TypInt -> "int"
  | TypFloat -> "float64"
  | TypComplex -> "complex128"
  | TypBool -> "bool"
  | TypString -> "string"

(** [typ_to_string typ] returns a string representation of the type [typ]. *)
let rec typ_to_string = function
  | TypBasic b -> basic_typ_to_string b
  | TypFunc (l, t) ->
      sprintf
        "func(%s) %s"
        (typ_list_to_string l)
        (option_to_string "" basic_typ_to_string t)

(** [typ_list_to_string l] returns a string representation of the type list [l]. *)
and typ_list_to_string = function
  | [] -> ""
  | [x] -> sprintf "%s" (typ_to_string x)
  | x :: r -> sprintf "%s %s" (typ_to_string x) (typ_list_to_string r)

(** [list_to_string l] returns a string representation of the list [l]. *)
let list_to_string f prefix l =
  let prefix' = prefix ^ indent in
  let rec lts = function
    | [] -> ""
    | [x] -> sprintf "%s%s%s" prefix' branch_end (f (prefix' ^ " ") x)
    | x :: r -> sprintf "%s%s%s\n%s" prefix' branch (f (prefix' ^ pipe) x) (lts r)
  in
  lts l

(** [expression_to_string prefix e] returns a string representation of the expression [e],
    where [prefix] is the string that prefixes [e]. *)
let rec expression_to_string prefix e = raw_expression_to_string prefix e.content

(** [raw_expression_to_string prefix e] returns a string representation of the raw expression [e],
    where [prefix] is the string that prefixes [e]. *)
and raw_expression_to_string prefix e =
  let prefix' = prefix ^ indent in
  match e with
  | ELiteral lit -> sprintf "ELiteral (%s)" (literal_to_string lit)
  | EIdentRef id -> sprintf "EIdentRef '%s'" id.content
  | EFuncCall (id, args) ->
      let args_as_string =
        if List.length args = 0 then ""
        else sprintf "\n%s" (expression_list_to_string prefix' args)
      in
      sprintf "EFuncCall '%s'\n%s%s()%s" id.content prefix' branch_end args_as_string
  | EConversion (t, e) ->
      sprintf
        "EConversion %s\n%s%s%s"
        (typ_to_string t)
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | EUnOp (uop, e) ->
      sprintf
        "EUnOp '%s'\n%s%s%s"
        (unop_to_string uop)
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | EBinOp (op, e1, e2) ->
      sprintf
        "EBinop '%s'\n%s%s%s\n%s%s%s"
        (binop_to_string op)
        prefix'
        branch
        (expression_to_string (prefix' ^ pipe) e1)
        prefix'
        branch_end
        (expression_to_string prefix' e2)

(** [expression_list_to_string prefix l] returns a string representation of the expression list [l],
    where [prefix] is the string that prefixes [l]. *)
and expression_list_to_string prefix l = list_to_string expression_to_string prefix l

(** [statement_to_string prefix s] returns a string representation of the statement [s],
    where [prefix] is the string that prefixes [s]. *)
let rec statement_to_string prefix s =
  let prefix' = prefix ^ indent in
  match s with
  | StVarDecl (id, t, e) ->
      let expr =
        match e with
        | None -> ""
        | Some v -> sprintf "\n%s%s%s" prefix' branch_end (expression_to_string prefix' v)
      in
      sprintf "StVarDecl '%s' %s%s" id.content (option_to_string "" typ_to_string t) expr
  | StConstDecl (id, t, e) ->
      sprintf
        "StConstDecl '%s' %s\n%s%s%s"
        id.content
        (option_to_string "untyped" typ_to_string t)
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | StIfElse (e, s1, s2) ->
      sprintf
        "StIfElse\n%s%s%s\n%s%s%s\n%s%s%s"
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
      sprintf
        "StWhileFor\n%s%s%s\n%s%s%s"
        prefix'
        branch
        (expression_to_string (prefix' ^ pipe) e)
        prefix'
        branch_end
        (statement_to_string prefix' s)
  | StAssign (id, e) ->
      sprintf
        "StAssign\n%s%s%s\n%s%s%s"
        prefix'
        branch
        id.content
        prefix'
        branch_end
        (expression_to_string prefix' e)
  | StBlock l -> sprintf "StBlock\n%s" (statement_list_to_string prefix l)
  | StPrintln args ->
      sprintf
        "StPrintln\n%s%s()\n%s"
        prefix'
        branch_end
        (expression_list_to_string prefix' args.content)

(** [statement_list_to_string prefix l] returns a string representation of the statement list [l],
    where [prefix] is the string that prefixes [l]. *)
and statement_list_to_string prefix l = list_to_string statement_to_string prefix l

(** [func_to_string prefix func] returns a string representation of the function [func],
    where [prefix] is the string that prefixes [func]. *)
let rec func_to_string prefix func =
  let prefix' = prefix ^ indent in
  let returned_expr =
    match func.return.content with
    | None -> ""
    | Some v ->
        sprintf
          "\n%s%s%s"
          (prefix' ^ indent)
          branch_end
          (expression_to_string (prefix' ^ indent) v)
  in
  sprintf
    "FuncDecl '%s'\n%s%sparams (%s)\n%s%sbody%s%s\n%s%sresult %s\n%s%sreturn%s"
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

(** [func_list_to_stirng prefix l] returns a string representation of the function list [l], 
    where [prefix] is the string that prefixes [l]. *)
and func_list_to_string prefix l = list_to_string func_to_string prefix l

(** [param_list_to_string l] returns a string representation of the function parameter list [l]. *)
and param_list_to_string = function
  | [] -> ""
  | [x] -> sprintf "%s %s" (fst x).content (typ_to_string @@ snd x)
  | x :: r ->
      sprintf
        "%s %s, %s"
        (fst x).content
        (typ_to_string @@ snd x)
        (param_list_to_string r)

(** [print prog] prints the abstract syntax tree [prog] on the standard output. *)
let print prog = printf "program\n%s\n" @@ func_list_to_string "" prog.defs
