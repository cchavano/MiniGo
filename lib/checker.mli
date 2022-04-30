exception Error of string

(** [check_program program] returns an [Ast_typ] program if the [Ast_loc] program is semantically correct. *)
val check_program : Ast_loc.program -> Ast_typ.program
