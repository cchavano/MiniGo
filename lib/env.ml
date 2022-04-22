open Ast_typ

type t = (string, record) Hashtbl.t

and record = {
  typ : typ;
  mode : expression_mode;
  value : expression_value option;
  mutable used : bool;
}

exception Undeclared_name of string

let lookup (id : string Location.t) (env : t) =
  try Hashtbl.find env id.content
  with Not_found ->
    raise @@ Undeclared_name (Printf.sprintf "undeclared name: %s" id.content)

let set_used (id : string Location.t) (env : t) =
  let record = lookup id env in
  record.used <- true;
  Hashtbl.replace env id.content record
