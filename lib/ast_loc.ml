type program = {
  package: identifier;
  import: string option;
  defs: func_def list
}

and func_def = {
  name: identifier;
  params: (identifier * typ) list;
  body: statement list;
  result: basic_typ option;
  return: expression option
}

and statement =
  | StVarDecl of identifier * typ option * expression option
  | StConstDecl of identifier * typ option * expression
  | StIfElse of expression * statement * statement
  | StWhileFor of expression * statement
  | StAssign of identifier * expression
  | StBlock of statement list
  | StPrintln of expression list

and expression = raw_expression Location.t

and raw_expression =
  | ELiteral of literal
  | EIdentRef of identifier
  | EFuncCall of identifier * expression list
  | EValueCast of basic_typ * expression
  | EUnOp of unary_op * expression
  | EBinOp of binary_op * expression * expression

and identifier = string Location.t

and typ =
  | TypBasic of basic_typ
  | TypFunc of (typ list) * basic_typ

and basic_typ =
  | TypInt
  | TypFloat
  | TypComplex
  | TypBool
  | TypString

and literal =
  | LitInt of int64
  | LitFloat of float
  | LitImag of float
  | LitBool of bool
  | LitString of string

and unary_op =
  | UOpNot
  | UOpPlus
  | UOpMinus

and binary_op =
  | OpArithmetic of arithmetic_op
  | OpCompare of compare_op
  | OpLogic of logic_op

and arithmetic_op =
  | OpPlus
  | OpMinus
  | OpMult
  | OpDiv

and compare_op =
  | OpLesst
  | OpGreat
  | OpEqual
  | OpNotEqual

and logic_op =
  | OpAnd
  | OpOr