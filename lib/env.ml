open Ast_typ

(* Environment used to track identifiers (functions, variables, constants) during the
   semantic verification of the program. *)

(** The type of the environment. It maps a string to a record. *)
type t = (string, record) Hashtbl.t

(** The type of a record contained in the environment. *)
and record = {
  typ : typ;
  mode : expression_mode;
  value : expression_value option;
  mutable used : bool;
}

exception Undeclared_name of string

(** [lookup id env] returns the current binding of [id] in [env], or raises
    [Undeclared_name] if no such binding exists. *)
let lookup (id : string Location.t) (env : t) =
  try Hashtbl.find env id.content
  with Not_found ->
    raise @@ Undeclared_name (Printf.sprintf "undeclared name: %s" id.content)

(** [set_used id env] updates the [used] field of the record binded to [id] in [env], or raises
    [Undeclared_name] if no such binding exists. *)
let set_used (id : string Location.t) (env : t) =
  let record = lookup id env in
  record.used <- true;
  Hashtbl.replace env id.content record
