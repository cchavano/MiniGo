%{
  open Ast_loc
%}

%token PACKAGE
%token IMPORT
%token FUNC
%token VAR CONST
%token FOR
%token IF ELSE
%token RETURN
%token INT FLOAT COMPLEX BOOL STRING
%token ASSIGN DEFINE
%token PLUS MINUS MULT DIV
%token LESST GREAT EQUAL NOT_EQUAL
%token AND OR
%token NOT
%token LPAREN RPAREN LBRACE RBRACE
%token COMMA SEMICOLON
%token PRINTLN
%token <int64> INT_LIT
%token <float> FLOAT_LIT
%token <float> IMAG_LIT
%token <bool> BOOL_LIT
%token <string> STRING_LIT
%token <string Location.t> IDENT
%token EOF

%left OR
%left AND
%left EQUAL NOT_EQUAL LESST GREAT
%left PLUS MINUS
%left MULT DIV
%nonassoc NOT

%start program
%type<program> program
%%

program:
  | PACKAGE pkg = IDENT SEMICOLON
    import = option(import)
    defs = list(func_def)
    EOF
    {
      {
        package = pkg;
        import = import;
        defs = defs
      }
    }

import:
  | IMPORT pkg_name = STRING_LIT SEMICOLON { Location.make $startpos $endpos pkg_name }

func_def:
  | FUNC name = IDENT
    LPAREN
    params = separated_list(COMMA, func_param)
    RPAREN
    result = option(basic_typ)
    LBRACE
    body = list(statement_ended)
    return = func_return
    RBRACE
    SEMICOLON
    {
      {
        name = name;
        params = params;
        body = body;
        result = result;
        return = return
      }
    }

func_param:
  | id = IDENT param_typ = typ { (id, param_typ) }

func_return:
  | { Location.make $startpos $endpos None }
  | RETURN return = option(expression) SEMICOLON { Location.make $startpos $endpos return }

statement_ended:
  | st = statement SEMICOLON { st }

statement:
  | VAR id = IDENT var_typ = typ { StVarDecl (id, Some var_typ, None) }
  | VAR id = IDENT opt_typ = option(typ) ASSIGN e = expression { StVarDecl (id, opt_typ, Some e) }
  | id = IDENT DEFINE e = expression { StVarDecl (id, None, Some e) }
  | CONST id = IDENT opt_typ = option(typ) ASSIGN e = expression { StConstDecl (id, opt_typ, e) }
  | IF cond = expression sb1 = statement_block ELSE sb2 = statement_block { StIfElse (cond, sb1, sb2) }
  | FOR cond = expression sb = statement_block { StWhileFor (cond, sb) }
  | id = IDENT ASSIGN e = expression { StAssign (id, e) }
  | PRINTLN LPAREN args = separated_list(COMMA, expression) RPAREN { StPrintln args }
  | block = statement_block { block }

statement_block:
  | LBRACE sts = list(statement_ended) RBRACE { StBlock sts }

expression:
  | e = raw_expression { Location.make $startpos $endpos e }
  | LPAREN e = expression RPAREN { e }

raw_expression:
  | lit = literal { ELiteral lit }
  | id = IDENT { EIdentRef id }
  | id = IDENT LPAREN args = separated_list(COMMA, expression) RPAREN { EFuncCall (id, args) }
  | op = unary_op e = expression { EUnOp (op, e) }
  | e1 = expression op = binary_op e2 = expression { EBinOp (op, e1, e2) }
  | typ = typ LPAREN e = expression RPAREN { EConversion (typ, e) }

typ:
  | basic_t = basic_typ { TypBasic basic_t }
  | func_t = func_typ { func_t }

basic_typ:
  | INT { TypInt }
  | FLOAT { TypFloat }
  | COMPLEX { TypComplex }
  | BOOL { TypBool }
  | STRING { TypString }

func_typ:  
  | FUNC
    LPAREN params_t = separated_list(COMMA, typ) RPAREN
    result_t = option(basic_typ) { TypFunc (params_t, result_t) }

literal:
  | i = INT_LIT { LitInt i }
  | f = FLOAT_LIT { LitFloat f }
  | im = IMAG_LIT { LitImag im }
  | b = BOOL_LIT { LitBool b }
  | s = STRING_LIT { LitString s }

%inline unary_op:
  | NOT { UOpNot }
  | PLUS { UOpPlus }
  | MINUS { UOpMinus }

%inline binary_op:
  | op = arithmetic_op { OpArithmetic op }
  | op = compare_op { OpCompare op }
  | op = logic_op { OpLogic op }

%inline arithmetic_op:
  | PLUS { OpAdd }
  | MINUS { OpSub }
  | MULT { OpMul }
  | DIV { OpDiv }

%inline compare_op:
  | LESST { OpLesst }
  | GREAT { OpGreat }
  | EQUAL { OpEqual }
  | NOT_EQUAL { OpNotEqual }

%inline logic_op:
  | AND { OpAnd }
  | OR { OpOr }