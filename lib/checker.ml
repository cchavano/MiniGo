open Ast_typ
open Location
open Printf
module StringMap = Map.Make (String)

module Complex = struct
  include Complex

  let of_int64 i = { re = Int64.to_float i; im = 0. }

  let of_float f = { re = f; im = 0. }
end

exception Error of string

exception Incompatible_assign

exception Invalid_conversion

exception Undefine_op

exception Mismatched_types

exception Too_many_values

exception Invalid_call

exception Invalid_const_init

exception Invalid_cond

exception Unassignable_operand

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
  | LitInt i -> Int64.to_string i
  | LitFloat f -> Float.to_string f
  | LitImag im -> Float.to_string im ^ "i"
  | LitBool b -> Bool.to_string b
  | LitString s -> s

(** [value_to_string v] returns the value contained in the [Ast_typ.expression_value] [v]. *)
let value_to_string = function
  | ValInt i -> Int64.to_string i
  | ValFloat f -> Float.to_string f
  | ValComplex c -> sprintf "%f*%f" c.re c.im
  | ValBool b -> Bool.to_string b
  | ValString s -> s

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

(** [expression_to_string prefix e] returns a string representation of the expression [e]. *)
let rec expression_to_string e = raw_expression_to_string e.raw

(** [raw_expression_to_string e] returns a string representation of the raw expression [e]. *)
and raw_expression_to_string = function
  | ELiteral lit -> literal_to_string lit
  | EIdentRef id -> id
  | EFuncCall (id, args) -> sprintf "%s(%s)" id (expression_list_to_string args)
  | EConversion (typ, e) -> sprintf "%s(%s)" (typ_to_string typ) (expression_to_string e)
  | EUnOp (uop, e) -> sprintf "%s%s" (unop_to_string uop) (expression_to_string e)
  | EBinOp (op, e1, e2) ->
      sprintf
        "%s %s %s"
        (expression_to_string e1)
        (binop_to_string op)
        (expression_to_string e2)

(** [expression_list_to_string l] returns a string representation of the expression list [l]. *)
and expression_list_to_string l =
  match l with
  | [] -> ""
  | [x] -> expression_to_string x
  | x :: r -> sprintf "%s, %s" (expression_to_string x) (expression_list_to_string r)

(** [expression_info_to_string e] returns a tiny description (as a string) of the expression [e]. *)
let expression_info_to_string e =
  let typ_as_string = typ_to_string e.typ in
  let value_or_empty e =
    match e.raw with
    | ELiteral _ -> ""
    | _ -> " " ^ value_to_string (Option.get e.value)
  in
  match e.mode with
  | ModUntyped -> sprintf "untyped %s constant%s" typ_as_string (value_or_empty e)
  | ModConstant -> sprintf "constant %s of type %s" (value_or_empty e) typ_as_string
  | ModVariable -> sprintf "variable of type %s" typ_as_string
  | ModValue -> sprintf "value of type %s" typ_as_string
  | _ -> assert false

(** [error loc msg] raises an exception [Error] with the message [msg] and the position information contained in [loc]. *)
let error loc msg =
  raise
  @@ Error (sprintf "%s %s" (Pretty_error.from_interval loc.startpos loc.endpos) msg)

(** [lookup id env] returns the current binding of [id] in [env], or raises
    an exception [Error] if no such binding exists. *)
let lookup id env = try Env.lookup id env with Env.Undeclared_name msg -> error id msg

(** [set_used id env] updates the [used] field of the record binded to [id] in [env], or raises
    an exception [Error] if no such binding exists. *)
let set_used id env =
  try Env.set_used id env with Env.Undeclared_name msg -> error id msg

(** [mkte raw typ mode value] builds a new [Ast_typ] expression with raw expression [raw],
    type [typ], mode [mode] and value [value]. *)
let mkte raw typ mode value = { raw; typ; mode; value }

(** [basic t] returns a [TypBasic] type from the basic type [t]. *)
let basic t = TypBasic t

(** [typeof_literal lit] returns the type of the literal [lit]. *)
let typeof_literal lit =
  let basic_t =
    match lit with
    | LitInt _ -> TypInt
    | LitFloat _ -> TypFloat
    | LitImag _ -> TypComplex
    | LitBool _ -> TypBool
    | LitString _ -> TypString
  in
  TypBasic basic_t

(** [valueof_literal lit] returns the value of the literal [lit]. *)
let valueof_literal = function
  | LitInt i -> ValInt i
  | LitFloat f -> ValFloat f
  | LitImag im -> ValComplex { re = 0.; im }
  | LitBool b -> ValBool b
  | LitString s -> ValString s

(** [is_untyped e] checks if the expression [e] is an untyped constant. *)
let is_untyped e = e.mode = ModUntyped

(** [is_const e] checks if the expression [e] is a constant. *)
let is_const e = e.mode = ModConstant || e.mode = ModUntyped

(** [representation value typ] returns the representation of [value] by [typ] if [value] is representable by
    a value of type [typ], else returns [None]. *)
let representation value typ =
  match (value, typ) with
  | (ValInt _ as vi), TypInt -> Some vi
  | (ValFloat _ as vf), TypFloat -> Some vf
  | (ValComplex _ as vc), TypComplex -> Some vc
  | (ValBool _ as vb), TypBool -> Some vb
  | (ValString _ as vs), TypString -> Some vs
  | ValInt i, TypFloat -> Some (ValFloat (Int64.to_float i))
  | ValInt i, TypComplex -> Some (ValComplex { re = Int64.to_float i; im = 0. })
  | ValFloat f, TypComplex -> Some (ValComplex { re = f; im = 0. })
  | ValFloat f, TypInt ->
      if Float.is_integer f then Some (ValInt (Int64.of_float f)) else None
  | ValComplex c, TypInt ->
      if c.im = 0. && Float.is_integer c.re then Some (ValInt (Int64.of_float c.re))
      else None
  | ValComplex c, TypFloat -> if c.im = 0. then Some (ValFloat c.re) else None
  | _ -> None

(** [const_basic_typ typ] returns [basic_t] if [typ] is [TypBasic basic_t], else [assert false]. *)
let const_basic_typ = function
  | TypBasic basic_t -> basic_t
  | TypFunc _ -> assert false

(** [is_representable typ e] checks if the constant expression [e] has
    a value that can be represented by a value of type [typ]. *)
let is_representable typ e =
  representation (Option.get e.value) (const_basic_typ typ) <> None

(** [check_assignability typ e] checks if the expression [e] is assignable to a variable of type [typ]. *)
let check_assignability typ e =
  if e.typ = typ || (is_untyped e && is_representable typ e) then ()
  else raise Incompatible_assign

(** [check_conversion typ e] returns an [Ast_typ.EConversion] expression if the type of [e] can
    be converted into [typ], else raises [Invalid_conversion]. *)
let check_conversion typ e =
  let value, mode =
    if is_const e then
      ( begin
          let v = Option.get e.value in
          let basic_t = const_basic_typ typ in
          if is_representable typ e then representation v basic_t
          else raise Invalid_conversion
        end,
        ModConstant )
    else
      let typ_compatibility =
        if e.typ = typ then true
        else
          match (typ, e.typ) with
          | TypBasic b1, TypBasic b2 -> (
              match (b1, b2) with
              | TypInt, TypFloat | TypFloat, TypInt -> true
              | _ -> false)
          | _ -> false
      in
      if typ_compatibility then (None, ModValue) else raise Invalid_conversion
  in
  mkte (EConversion (typ, e)) typ mode value

(** [check_unary_op op e] returns an [Ast_typ.EUnOp] expression if the unary operation
    between the operator [op] and the expression [e] is valid. *)
let check_unary_op op e =
  let typ =
    match e.typ with
    | TypBasic basic_t ->
        basic
          (match (op, basic_t) with
          | UOpNot, TypBool -> TypBool
          | (UOpPlus | UOpMinus), ((TypInt | TypFloat | TypComplex) as t) -> t
          | _ -> raise Undefine_op)
    | TypFunc _ -> raise Undefine_op
  in
  let value =
    if is_const e then
      Some
        (match (op, Option.get e.value) with
        | UOpNot, ValBool b -> ValBool (not b)
        | UOpPlus, ((ValInt _ | ValFloat _ | ValComplex _) as num) -> num
        | UOpMinus, ValInt i -> ValInt (Int64.mul Int64.minus_one i)
        | UOpMinus, ValFloat f -> ValFloat (-.f)
        | UOpMinus, ValComplex c -> ValComplex (Complex.neg c)
        | _ -> raise Mismatched_types)
    else None
  in
  mkte (EUnOp (op, e)) typ e.mode value

(** [eval_arithmetic op v1 v2] returns an [Ast_typ.expression_value] that contains the
    result of the arithmetic operation [op] between values [v1] and [v2].
    Raises [Undefine_op] if the operator [op] is not defined for both of the operands. 
    Raises [Mismatched_types] if [v1] and [v2] are not valid operands for [op]. *)
let eval_arithmetic op v1 v2 =
  let eval_test v1 v2 fint64 ffloat fcomplex =
    match (v1, v2) with
    | ValInt i1, ValInt i2 -> ValInt (fint64 i1 i2)
    | ValFloat f1, ValFloat f2 -> ValFloat (ffloat f1 f2)
    | ValComplex c1, ValComplex c2 -> ValComplex (fcomplex c1 c2)
    | ValInt i, ValFloat f -> ValFloat (ffloat (Int64.to_float i) f)
    | ValFloat f, ValInt i -> ValFloat (ffloat f (Int64.to_float i))
    | ValInt i, ValComplex c ->
        let c' = Complex.of_int64 i in
        ValComplex (fcomplex c' c)
    | ValComplex c, ValInt i ->
        let c' = Complex.of_int64 i in
        ValComplex (fcomplex c c')
    | ValFloat f, ValComplex c ->
        let c' = Complex.of_float f in
        ValComplex (fcomplex c' c)
    | ValComplex c, ValFloat f ->
        let c' = Complex.of_float f in
        ValComplex (fcomplex c c')
    | ValString s1, ValString s2 ->
        if op = OpAdd then ValString (s1 ^ s2) else raise Undefine_op
    | _ -> raise Mismatched_types
  in
  match op with
  | OpAdd -> eval_test v1 v2 Int64.add Float.add Complex.add
  | OpSub -> eval_test v1 v2 Int64.sub Float.sub Complex.sub
  | OpMul -> eval_test v1 v2 Int64.mul Float.mul Complex.mul
  | OpDiv -> eval_test v1 v2 Int64.div Float.div Complex.div

(** [check_arithmetic op e1 e2] returns an [Ast_typ.EBinOp] arithmetic expression if [e1] and [e2]
    are valid operands for the operator [op], else raises [Mismatched_types]. *)
let check_arithmetic op e1 e2 =
  let check_basic op t1 t2 =
    match (t1, t2) with
    | TypBasic b1, TypBasic b2 ->
        basic
          (match (op, b1, b2) with
          | _, TypInt, TypInt -> TypInt
          | _, TypFloat, TypFloat -> TypFloat
          | _, TypComplex, TypComplex -> TypComplex
          | OpAdd, TypString, TypString -> TypString
          | _ -> raise Mismatched_types)
    | _ -> raise Mismatched_types
  in
  let check_untyped op t1 t2 =
    let t1', t2' = (const_basic_typ t1, const_basic_typ t2) in
    match (t1', t2') with
    | TypInt, TypFloat | TypFloat, TypInt -> basic TypFloat
    | TypInt, TypComplex
    | TypComplex, TypInt
    | TypFloat, TypComplex
    | TypComplex, TypFloat -> basic TypComplex
    | _ -> check_basic op t1 t2
  in
  let typ =
    if is_untyped e1 || is_untyped e2 then check_untyped op e1.typ e2.typ
    else check_basic op e1.typ e2.typ
  in
  let value, mode =
    match (e1.mode, e2.mode) with
    | ModUntyped, ModUntyped ->
        let v1, v2 = (Option.get e1.value, Option.get e2.value) in
        (Some (eval_arithmetic op v1 v2), ModUntyped)
    | (ModConstant | ModUntyped), (ModConstant | ModUntyped) ->
        let v1, v2 = (Option.get e1.value, Option.get e2.value) in
        (Some (eval_arithmetic op v1 v2), ModConstant)
    | _ -> (None, ModValue)
  in
  mkte (EBinOp (OpArithmetic op, e1, e2)) typ mode value

(** [eval_comparison op v1 v2] returns a [Ast_typ.expression_value] that contains the
    result of the comparison operation [op] between values [v1] and [v2].
    Raises [Mismatched_types] if [v1] and [v2] can't be compared with [op]. *)
let eval_comparison op v1 v2 =
  let eval_eq v1 v2 =
    match (v1, v2) with
    | ValInt i1, ValInt i2 -> ValBool (i1 = i2)
    | ValFloat f1, ValFloat f2 -> ValBool (f1 = f2)
    | ValComplex c1, ValComplex c2 -> ValBool (c1 = c2)
    | ValBool b1, ValBool b2 -> ValBool (b1 = b2)
    | ValString s1, ValString s2 -> ValBool (s1 = s2)
    | ValInt i, ValComplex c | ValComplex c, ValInt i ->
        let f = Int64.to_float i in
        ValBool (c.im = 0. && c.re = f)
    | ValFloat f, ValComplex c | ValComplex c, ValFloat f ->
        ValBool (c.im = 0. && c.re = f)
    | _ -> raise Mismatched_types
  in
  let eval_neq v1 v2 =
    match eval_eq v1 v2 with
    | ValBool b -> ValBool (not b)
    | _ -> assert false
  in
  let eval_lt v1 v2 =
    match (v1, v2) with
    | ValInt i1, ValInt i2 -> ValBool (i1 < i2)
    | ValFloat f1, ValFloat f2 -> ValBool (f1 < f2)
    | _ -> raise Mismatched_types
  in
  let eval_gt v1 v2 =
    match (v1, v2) with
    | ValInt i1, ValInt i2 -> ValBool (i1 > i2)
    | ValFloat f1, ValFloat f2 -> ValBool (f1 > f2)
    | _ -> raise Mismatched_types
  in
  match op with
  | OpEqual -> eval_eq v1 v2
  | OpNotEqual -> eval_neq v1 v2
  | OpLesst -> eval_lt v1 v2
  | OpGreat -> eval_gt v1 v2

(** [check_comparison op e1 e2] returns an [Ast_typ.EBinOp] comparison expression if [e1] and [e2]
    are valid operands for the operator [op], elses raises [Mismatched_types]. *)
let check_comparison op e1 e2 =
  let check_basic op t1 t2 =
    match (t1, t2) with
    | TypBasic b1, TypBasic b2 ->
        basic
          (match (op, b1, b2) with
          | _, TypInt, TypInt
          | _, TypFloat, TypFloat
          | (OpEqual | OpNotEqual), TypComplex, TypComplex
          | (OpEqual | OpNotEqual), TypBool, TypBool
          | _, TypString, TypString -> TypBool
          | _ -> raise Mismatched_types)
    | _ -> raise Mismatched_types
  in
  let check_untyped op t1 t2 =
    let t1', t2' = (const_basic_typ t1, const_basic_typ t2) in
    match (op, t1', t2') with
    | ( (OpEqual | OpNotEqual),
        (TypInt | TypFloat | TypComplex),
        (TypInt | TypFloat | TypComplex) ) -> basic TypBool
    | _ -> check_basic op t1 t2
  in
  let typ =
    if is_untyped e1 || is_untyped e2 then check_untyped op e1.typ e2.typ
    else check_basic op e1.typ e2.typ
  in
  let value, mode =
    if is_const e1 && is_const e2 then
      let v1, v2 = (Option.get e1.value, Option.get e2.value) in
      (Some (eval_comparison op v1 v2), ModUntyped)
    else (None, ModValue)
  in
  mkte (EBinOp (OpCompare op, e1, e2)) typ mode value

(** [eval_logic op v1 v2] returns a [Ast_typ.expression_value] that contains the
    result of the logical operation [op] between values [v1] and [v2].
    Raises [Mismatched_types] if [v1] and [v2] are not valid operands for [op]. *)
let eval_logic op v1 v2 =
  let eval_test v1 v2 f =
    match (v1, v2) with
    | ValBool b1, ValBool b2 -> ValBool (f b1 b2)
    | _ -> raise Mismatched_types
  in
  match op with
  | OpAnd -> eval_test v1 v2 ( && )
  | OpOr -> eval_test v1 v2 ( || )

(** [check_logic op e1 e2] returns an [Ast_typ.EBinOp] logical expression if [e1] and [e2]
    are valid operands for the operator [op], else raises [Mismatched_types]. *)
let check_logic op e1 e2 =
  let typ =
    match (e1.typ, e2.typ) with
    | TypBasic b1, TypBasic b2 ->
        basic
          (match (b1, b2) with
          | TypBool, TypBool -> TypBool
          | _ -> raise Mismatched_types)
    | _ -> raise Mismatched_types
  in
  let value, mode =
    match (e1.mode, e2.mode) with
    | ModUntyped, ModUntyped ->
        let v1, v2 = (Option.get e1.value, Option.get e2.value) in
        (Some (eval_logic op v1 v2), ModUntyped)
    | (ModConstant | ModUntyped), (ModUntyped | ModConstant) ->
        let v1, v2 = (Option.get e1.value, Option.get e2.value) in
        (Some (eval_logic op v1 v2), ModUntyped)
    | _ -> (None, ModValue)
  in
  mkte (EBinOp (OpLogic op, e1, e2)) typ mode value

(** [valueof_builtin_len e] returns an [Ast_typ.expression_value] that contains the result of
    the Go "len" builtin function applied to the expression [e]. *)
let valueof_builtin_len e =
  let value = representation (Option.get e.value) TypString in
  match value with
  | Some v -> begin
      match v with
      | ValString s -> ValInt (Int64.of_int (String.length s))
      | _ -> assert false
    end
  | None -> assert false

(** [valueof_builtin_complex e2 e2] returns an [Ast_typ.expression_value] that contains the result of
    the Go "complex" builtin function applied to the expressions [e1] and [e2]. *)
let valueof_builtin_complex e1 e2 =
  let value1 = representation (Option.get e1.value) TypFloat in
  let value2 = representation (Option.get e2.value) TypFloat in
  match (value1, value2) with
  | Some v1, Some v2 -> begin
      match (v1, v2) with
      | ValFloat f1, ValFloat f2 -> ValComplex { re = f1; im = f2 }
      | _ -> assert false
    end
  | _ -> assert false

(** [valueof_builtin_real e] returns an [Ast_typ.expression_value] that contains the result of
    the Go "real" builtin function applied to the expression [e]. *)
let valueof_builtin_real e =
  let value = representation (Option.get e.value) TypComplex in
  match value with
  | Some v -> begin
      match v with
      | ValComplex c -> ValFloat c.re
      | _ -> assert false
    end
  | None -> assert false

(** [valueof_builtin_imag e] returns an [Ast_typ.expression_value] that contains the result of
    the Go "imag" builtin function applied to the expression [e]. *)
let valueof_builtin_imag e =
  let value = representation (Option.get e.value) TypComplex in
  match value with
  | Some v -> begin
      match v with
      | ValComplex c -> ValFloat c.im
      | _ -> assert false
    end
  | None -> assert false

(** [valueof_builtin name args] returns an [Ast_typ.expression_value] that contains the result of
    the Go builtin function named [name] applied to the arguments [args]. *)
let valueof_builtin name args =
  match name with
  | "len" -> valueof_builtin_len (List.nth args 0)
  | "real" -> valueof_builtin_real (List.nth args 0)
  | "imag" -> valueof_builtin_imag (List.nth args 0)
  | "complex" -> valueof_builtin_complex (List.nth args 0) (List.nth args 1)
  | _ -> assert false

(** [check_func_call id args env] returns an [Ast_typ.EFuncCall] expression if the call of the
    function [id] with the arguments [args] is valid when made in the environment [env].
    Raises [Too_many_values] if the function has no return type.
    Raises [Invalid_call] if [id] does not refer to a function (directly of via a variable).
    Raises [Error] with the appropriate message if the arguments [args] don't match the function signature. *)
let rec check_func_call id args env =
  let record = lookup id env in
  match record.typ with
  | TypFunc (params_t, t) ->
      let args' = List.map (check_expression env) args in
      let arg_index = ref (-1) in
      begin
        try
          List.iter2
            (fun e typ ->
              arg_index := !arg_index + 1;
              check_assignability typ e)
            args'
            params_t
        with
        | Incompatible_assign ->
            let arg_loc, arg_typ =
              (List.nth args !arg_index, List.nth args' !arg_index)
            in
            let expected_typ = List.nth params_t !arg_index in
            error
              arg_loc
              (sprintf
                 "cannot use %s (%s) as %s value in argument to %s"
                 (expression_to_string arg_typ)
                 (expression_info_to_string arg_typ)
                 (typ_to_string expected_typ)
                 id.content)
        | Invalid_argument _ ->
            let prefix =
              if List.length params_t > List.length args then "not enough arguments"
              else "too many arguments"
            in
            let startpos, endpos =
              let arg_count = List.length args in
              if arg_count > 0 then
                ((List.nth args 0).startpos, (List.nth args (arg_count - 1)).endpos)
              else
                ( { id.endpos with pos_cnum = id.endpos.pos_cnum + 1 },
                  { id.endpos with pos_cnum = id.endpos.pos_cnum + 2 } )
            in
            let loc = { startpos; endpos; content = args } in
            error
              loc
              (sprintf
                 "invalid operation: %s in call to %s (expected %i, found %i)"
                 prefix
                 id.content
                 (List.length params_t)
                 (List.length args))
      end;
      if Option.is_none t then raise Too_many_values;
      set_used id env;
      let typ = basic (Option.get t) in
      let mode =
        let args_mode =
          if List.length args' = 0 then ModValue
          else
            List.fold_left
              (fun acc arg ->
                if (acc = ModConstant || acc = ModUntyped) && is_const arg then
                  ModConstant
                else arg.mode)
              (List.nth args' 0).mode
              args'
        in
        match (record.mode, args_mode) with
        | ModBuiltin, ModConstant -> ModConstant
        | _ -> ModValue
      in
      let value =
        match (record.mode, mode) with
        | ModBuiltin, ModConstant -> Some (valueof_builtin id.content args')
        | _ -> None
      in
      mkte (Ast_typ.EFuncCall (id.content, args')) typ mode value
  | _ -> raise Invalid_call

(** [check_expression env e] returns a typed expression if the [Ast_loc] expression [e] is valid
    given the environment [env]. *)
and check_expression env e =
  match e.content with
  | Ast_loc.ELiteral lit ->
      mkte (ELiteral lit) (typeof_literal lit) ModUntyped (Some (valueof_literal lit))
  | Ast_loc.EIdentRef id ->
      let record = lookup id env in
      if record.mode = ModBuiltin then
        error id (sprintf "%s (built-in) must be called" id.content);
      set_used id env;
      mkte (EIdentRef id.content) record.typ record.mode record.value
  | Ast_loc.EConversion (typ, e) -> (
      let e' = check_expression env e in
      try check_conversion typ e'
      with Invalid_conversion ->
        error
          e
          (sprintf
             "cannot convert %s (%s) to %s"
             (expression_to_string e')
             (expression_info_to_string e')
             (typ_to_string typ)))
  | Ast_loc.EFuncCall (id, args) -> begin
      try check_func_call id args env with
      | Too_many_values -> error e (sprintf "%s (no value) used as value" id.content)
      | Invalid_call ->
          let record = lookup id env in
          let expr =
            {
              raw = EIdentRef id.content;
              typ = record.typ;
              mode = record.mode;
              value = record.value;
            }
          in
          error
            e
            (sprintf
               "invalid operation: cannot call non-function %s (%s)"
               id.content
               (expression_info_to_string expr))
    end
  | Ast_loc.EUnOp (op, e) -> (
      let e' = check_expression env e in
      try check_unary_op op e'
      with Undefine_op ->
        error
          e
          (sprintf
             "invalid operation: operator %s not defined on %s (%s)"
             (unop_to_string op)
             (expression_to_string e')
             (expression_info_to_string e')))
  | Ast_loc.EBinOp (op, e1, e2) -> (
      let e1', e2' = (check_expression env e1, check_expression env e2) in
      try
        match op with
        | OpArithmetic op -> check_arithmetic op e1' e2'
        | OpCompare op -> check_comparison op e1' e2'
        | OpLogic op -> check_logic op e1' e2'
      with Mismatched_types ->
        let typ_as_string e =
          match e.mode with
          | ModUntyped -> "untyped " ^ typ_to_string e.typ
          | _ -> typ_to_string e.typ
        in
        error
          e
          (sprintf
             "invalid operation: %s (mismatched types %s and %s)"
             (raw_expression_to_string @@ EBinOp (op, e1', e2'))
             (typ_as_string e1')
             (typ_as_string e2')))

(** [check_var_decl id typ e] returns a [Ast_typ.statement] variable declaration if the declared variable [id]
    of type [typ] can be initialized with the expression [e]. *)
let check_var_decl id typ e =
  let name = id.content in
  match typ with
  | Some t -> (
      match e with
      | None -> (StVarDecl (name, t, None), t)
      | Some e' ->
          check_assignability t e';
          (StVarDecl (name, t, e), t))
  | None -> (
      match e with
      | None -> failwith "the generated AST doest not respect the language grammar"
      | Some e' -> (StVarDecl (name, e'.typ, e), e'.typ))

(** [check_const_decl id typ e] returns a [Ast_typ.statement] constant declaration if the declared constant [id]
    of type [typ] can be initialized with the expression [e]. *)
let check_const_decl id typ e =
  if not (is_const e) then raise Invalid_const_init
  else
    let name = id.content in
    match typ with
    | Some t ->
        check_assignability t e;
        (StConstDecl (name, t, e), t, ModConstant, e.value)
    | None -> (StConstDecl (name, e.typ, e), e.typ, ModUntyped, e.value)

(** [add_decl id vdecl] adds the variable / constant [id] in the map of declared variables / constants [vdecl]
    and then returns the updated map. Raises [Error] if the identifier [id] is already mapped. *)
let add_decl id vdecl =
  if not (StringMap.mem id.content vdecl) then
    StringMap.add id.content (id.startpos, id.endpos) vdecl
  else error id (sprintf "%s redeclared in this block" id.content)

(** [check_cond e] checks if the type of the condition [e] is boolean. *)
let check_cond e = if e.typ <> TypBasic TypBool then raise Invalid_cond else ()

(** [check_statement env vdecl s] returns a typed statement if the [Ast_loc] statement [s] is valid given the environment [env] and
    the map of variables and constants [vdecl] declared in the current block. *)
let rec check_statement env vdecl = function
  | Ast_loc.StVarDecl (id, typ, e) -> begin
      let e' =
        if Option.is_none e then None else Some (check_expression env (Option.get e))
      in
      try
        let statement, typ' = check_var_decl id typ e' in
        Hashtbl.add
          env
          id.content
          { typ = typ'; mode = ModVariable; value = None; used = false };
        (statement, add_decl id vdecl)
      with Incompatible_assign ->
        error
          (Option.get e)
          (sprintf
             "cannot use %s (%s) as %s value in variable declaration"
             (expression_to_string @@ Option.get e')
             (expression_info_to_string @@ Option.get e')
             (typ_to_string @@ Option.get typ))
    end
  | Ast_loc.StConstDecl (id, typ, e) -> begin
      let e' = check_expression env e in
      try
        let statement, typ', mode, value = check_const_decl id typ e' in
        Hashtbl.add env id.content { typ = typ'; mode; value; used = false };
        (statement, add_decl id vdecl)
      with
      | Incompatible_assign ->
          error
            e
            (sprintf
               "cannot use %s (%s) as %s value in variable declaration"
               (expression_to_string e')
               (expression_info_to_string e')
               (typ_to_string (Option.get typ)))
      | Invalid_const_init ->
          error
            e
            (sprintf
               "%s (%s) is not constant"
               (expression_to_string e')
               (expression_info_to_string e'))
    end
  | Ast_loc.StIfElse (e, s1, s2) -> begin
      let e' = check_expression env e in
      try
        check_cond e';
        let s1', _ = check_statement env StringMap.empty s1 in
        let s2', _ = check_statement env StringMap.empty s2 in
        (StIfElse (e', s1', s2'), vdecl)
      with Invalid_cond -> error e "non-boolean condition in if statement"
    end
  | Ast_loc.StWhileFor (e, s) -> begin
      let e' = check_expression env e in
      try
        check_cond e';
        let s', _ = check_statement env StringMap.empty s in
        (StWhileFor (e', s'), vdecl)
      with Invalid_cond -> error e "non-boolean condition in for statement"
    end
  | Ast_loc.StAssign (id, e) -> begin
      let e' = check_expression env e in
      let record = lookup id env in
      try
        match record.mode with
        | ModConstant | ModUntyped -> raise Unassignable_operand
        | _ ->
            check_assignability record.typ e';
            (StAssign (id.content, e'), vdecl)
      with
      | Unassignable_operand ->
          let ident =
            {
              raw = EIdentRef id.content;
              typ = record.typ;
              mode = record.mode;
              value = record.value;
            }
          in
          error
            id
            (sprintf
               "cannot assign to %s (%s)"
               (expression_to_string ident)
               (expression_info_to_string ident))
      | Incompatible_assign ->
          error
            e
            (sprintf
               "cannot use %s (%s) as %s value in assignment"
               (expression_to_string e')
               (expression_info_to_string e')
               (typ_to_string record.typ))
    end
  | Ast_loc.StBlock sl ->
      let sl', vdecl' = check_statement_list env StringMap.empty sl in
      check_used_var env vdecl';
      StringMap.iter (fun key _ -> Hashtbl.remove env key) vdecl';
      (StBlock sl', vdecl)
  | Ast_loc.StPrintln args ->
      let args' =
        List.fold_left
          (fun acc e ->
            let e' = check_expression env e in
            acc @ [e'])
          []
          args
      in
      (StPrintln args', vdecl)

(** [check_statement_list  env vdecl sl] returns a typed statement list if the list of [Ast_loc] statements [sl] is valid
    given the environment [env] and the map of variables and constants [vdecl] declared in the current block. *)
and check_statement_list (env : Env.t) vdecl sl =
  List.fold_left
    (fun (stl, vdecl1) st ->
      let st', vdecl1' = check_statement env vdecl1 st in
      (stl @ [st'], vdecl1'))
    ([], vdecl)
    sl

(** [check_used_var env vdecl] checks if all declared variables in [vdecl] have been used given the environment [env]. *)
and check_used_var env vdecl =
  StringMap.iter
    (fun name pos ->
      let (record : Env.record) = Hashtbl.find env name in
      if record.mode = ModVariable && not record.used then
        error
          { startpos = fst pos; endpos = snd pos; content = name }
          (sprintf "%s declared but not used" name)
      else ())
    vdecl

(** [check_func env func] returns an [Ast_typ] function if the [Ast_loc] function definition [func]
    is valid within the environment [env].*)
let check_func env (func : Ast_loc.func) =
  let params = List.map (fun (id, typ) -> (id.content, typ)) func.params in
  List.map
    (fun p ->
      (fst p, Env.{ typ = snd p; mode = ModVariable; value = None; used = false }))
    params
  |> List.to_seq |> Hashtbl.add_seq env;
  let check_func_res (func : Ast_loc.func) =
    match func.result with
    | None ->
        if Option.is_some func.return.content then
          error
            (Option.get func.return.content)
            (sprintf "function %s should not return a value" func.name.content)
        else None
    | Some t -> (
        if Option.is_none func.return.content then
          error
            func.return
            (sprintf
               "function %s should return a value of type %s"
               func.name.content
               (basic_typ_to_string t))
        else
          let return' = check_expression env (Option.get func.return.content) in
          try
            check_assignability (basic t) return';
            Some return'
          with Incompatible_assign ->
            error
              func.return
              (sprintf
                 "cannot use %s (%s) as %s value in return statement"
                 (expression_to_string return')
                 (expression_info_to_string return')
                 (basic_typ_to_string t)))
  in
  let body, vdecl = check_statement_list env StringMap.empty func.body in
  let return = check_func_res func in
  check_used_var env vdecl;
  { name = func.name.content; params; body; result = func.result; return }

(** [check_program program] returns an [Ast_typ] program if the [Ast_loc] program is semantically correct. *)
let check_program (program : Ast_loc.program) =
  if program.package.content <> "main" then
    error program.package "package command-line-arguments is not a main package";
  let check_import =
    match program.import with
    | Some import ->
        if import.content <> "\"fmt\"" then
          error import (sprintf "cannot resolve package reference %s" import.content)
        else Some import.content
    | None -> None
  in
  let get_func_typ (func : Ast_loc.func) =
    TypFunc
      (List.fold_left (fun acc param -> acc @ [snd param]) [] func.params, func.result)
  in
  let (env : Env.t) = Hashtbl.create @@ (4 + List.length program.defs) in
  List.iter
    (fun (n, t) ->
      Hashtbl.add env n { typ = t; mode = ModBuiltin; value = None; used = false })
    [
      ("len", TypFunc ([basic TypString], Some TypInt));
      ("real", TypFunc ([basic TypComplex], Some TypFloat));
      ("imag", TypFunc ([basic TypComplex], Some TypFloat));
      ("complex", TypFunc ([basic TypFloat; basic TypFloat], Some TypComplex));
    ];
  let try_add_func env (func : Ast_loc.func) =
    let add_func env (func : Ast_loc.func) =
      Hashtbl.add
        env
        func.name.content
        Env.{ typ = get_func_typ func; mode = ModValue; value = None; used = false }
    in
    match Hashtbl.find_opt env func.name.content with
    | None -> add_func env func
    | Some f ->
        if f.mode = ModBuiltin then add_func env func
        else error func.name (sprintf "%s redeclared in this block" func.name.content)
  in
  List.iter (fun f -> try_add_func env f) program.defs;
  if not (Hashtbl.mem env "main") then
    error program.package "function main is undeclared in the main package";
  let defs = List.map (fun f -> check_func env f) program.defs in
  { package = program.package.content; import = check_import; defs }
