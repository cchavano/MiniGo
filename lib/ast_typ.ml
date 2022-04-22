type program = {
  package : identifier;
  import : string option;
  defs : func list;
}

and func = {
  name : identifier;
  params : (identifier * typ) list;
  body : statement list;
  result : basic_typ option;
  return : expression option;
}

and statement =
  | StVarDecl of identifier * typ * expression option
  | StConstDecl of identifier * typ * expression
  | StIfElse of expression * statement * statement
  | StWhileFor of expression * statement
  | StAssign of identifier * expression
  | StBlock of statement list
  | StPrintln of expression list

and expression = {
  raw : raw_expression;
  typ : typ;
  mode : expression_mode;
  value : expression_value option;
}

and expression_value =
  | ValInt of int64
  | ValFloat of float
  | ValComplex of Complex.t
  | ValBool of bool
  | ValString of string

and expression_mode =
  | ModConstant
  | ModVariable
  | ModValue
  | ModNoValue
  | ModUntyped
  | ModBuiltin

and raw_expression =
  | ELiteral of literal
  | EIdentRef of identifier
  | EFuncCall of identifier * expression list
  | EConversion of typ * expression
  | EUnOp of unary_op * expression
  | EBinOp of binary_op * expression * expression

and identifier = string

and typ = Ast_loc.typ =
  | TypBasic of basic_typ
  | TypFunc of typ list * basic_typ option

and basic_typ = Ast_loc.basic_typ =
  | TypInt
  | TypFloat
  | TypComplex
  | TypBool
  | TypString

and literal = Ast_loc.literal =
  | LitInt of int64
  | LitFloat of float
  | LitImag of float
  | LitBool of bool
  | LitString of string

and unary_op = Ast_loc.unary_op =
  | UOpNot
  | UOpPlus
  | UOpMinus

and binary_op = Ast_loc.binary_op =
  | OpArithmetic of arithmetic_op
  | OpCompare of compare_op
  | OpLogic of logic_op

and arithmetic_op = Ast_loc.arithmetic_op =
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv

and compare_op = Ast_loc.compare_op =
  | OpLesst
  | OpGreat
  | OpEqual
  | OpNotEqual

and logic_op = Ast_loc.logic_op =
  | OpAnd
  | OpOr
