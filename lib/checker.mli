exception Error of string

val is_const : Ast_typ.expression -> bool

val check_program : Ast_loc.program -> Ast_typ.program
