(** [program_2c out program] transpiles the Go program [program] to C and output the result on [out]. *)
val program_2c : out_channel -> Ast_typ.program -> unit
