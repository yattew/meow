open Ast
open Object

let is_truthy obj =
  match obj with
  | Integer i -> if i = 0 then false else true
  | True -> true
  | False -> false
  | Null -> false
  | Function _ -> raise (Failure "cannot use operator on a function expression")
  | Builtin _ -> raise (Failure "cannot use operator on a builtin")
  | Return_value _ -> raise (Failure "cannot use operator on a return")
;;

let eval_bang_operator_expression _ = function
  | Integer i when i = 0 -> True
  | Integer _ -> False
  | True -> False
  | False -> True
  | Null -> True
  | Function _ ->
    raise (Failure "cannot use ! operator on a function expression")
  | Return_value _ -> raise (Failure "cannot use ! operator on a return")
  | Builtin _ -> raise (Failure "cannot use ! operator on a builtin")
;;

let eval_minus_operator_expression _ = function
  | Integer i -> Integer (-i)
  | x ->
    raise
      (Failure
         (Printf.sprintf
            "cannot use - operator with %s"
            (string_of_object_type x)))
;;

let eval_prefix_expression env op right =
  match op with
  | "!" -> eval_bang_operator_expression env right
  | "-" -> eval_minus_operator_expression env right
  | s -> raise (Failure (Printf.sprintf "cannot use %s as a prefix operator" s))
;;

let eval_integer_infix_expression i1 op i2 =
  match op with
  | "+" -> Integer (i1 + i2)
  | "-" -> Integer (i1 - i2)
  | "*" -> Integer (i1 * i2)
  | "/" -> Integer (i1 / i2)
  | "<" -> object_type_of_bool (i1 < i2)
  | ">" -> object_type_of_bool (i1 > i2)
  | "==" -> object_type_of_bool (i1 = i2)
  | "!=" -> object_type_of_bool (i1 != i2)
  | _ ->
    raise (Failure (Printf.sprintf "cannot use %s operator with integers" op))
;;

let eval_infix_bool_expression l op r =
  match op with
  | "==" -> object_type_of_bool (l = r)
  | "!=" -> object_type_of_bool (l != r)
  | _ ->
    raise (Failure (Printf.sprintf "cannot use %s operator with booleans" op))
;;

let eval_infix_expression left op right =
  match left, right with
  | Integer i1, Integer i2 -> eval_integer_infix_expression i1 op i2
  | True, True -> eval_infix_bool_expression True op True
  | True, False -> eval_infix_bool_expression True op False
  | False, True -> eval_infix_bool_expression False op True
  | False, False -> eval_infix_bool_expression False op False
  | _ -> raise (Failure "infix operators not implemented for non integer types")
;;

let rec eval_if_expression env = function
  | If_expression { condition; consequence; alternative } ->
    if is_truthy @@ eval_expression env condition
    then eval_block_statement env consequence
    else eval_block_statement env alternative
  | _ -> raise (Failure "can't be reached")

and eval_expression (env : Object.env) node =
  let open Ast in
  match node with
  | Integer_expression i -> Integer i
  | Boolean_expression b -> if b then True else False
  | Prefix_expression (op, expr) ->
    eval_expression env expr |> eval_prefix_expression env op
  | Infix_expression (l_expr, op, r_expr) ->
    let l_evaluated = eval_expression env l_expr in
    let r_evaluated = eval_expression env r_expr in
    eval_infix_expression l_evaluated op r_evaluated
  | If_expression expr -> eval_if_expression env (If_expression expr)
  | Identifier_expression id ->
    (match get env id with
     | Some obj -> obj
     | None ->
       raise
         (Failure (Printf.sprintf "Identifier %s not found in enviornment" id)))
  | Fn_expression { parameters; body } -> Function { parameters; body; env }
  | Call_expression { func_exp; args } ->
    let fn = eval_expression env func_exp in
    let evaluated_args = List.map (eval_expression env) args in
    (match fn with
     | Function { parameters; body; env = internal_env } ->
       let rec env_table_of_args names objects =
         match names, objects with
         | [], [] -> init_env
         | name :: names, obj :: objects ->
           add (env_table_of_args names objects) name obj
         | _ -> raise (Failure "unreachable, number of arguments != params")
       in
       let param_env = env_table_of_args parameters evaluated_args in
       let fn_env = extend_env param_env @@ extend_env env internal_env in
       (match eval_block_statement fn_env body with
        | Return_value obj -> obj
        | obj -> obj)
     | Builtin fn -> fn env evaluated_args
     | _ -> raise (Failure "only functions and builtins can be called"))
  | _ -> raise (Failure "unknown expression found")

and eval_statement env = function
  | Let_statement (id, exp) -> eval_expression env exp |> add env id, Null
  | Return_statement expr -> env, Return_value (eval_expression env expr)
  | Expression_statement expr -> env, eval_expression env expr

and eval_statements env stmts =
  let rec aux env prev = function
    | [] -> env, prev
    | x :: xs ->
      let env', evaluated_obj = eval_statement env x in
      (match evaluated_obj with
       | Return_value v -> env', Return_value v
       | _ -> aux env' evaluated_obj xs)
  in
  aux env Null stmts

and eval_block_statement env stmts =
  match eval_statements env stmts with
  | _, res -> res

and eval_program env program =
  let rec aux env prev program =
    match program with
    | [] -> env, prev
    | x :: xs ->
      let env', evaluated_obj = eval_statement env x in
      (match evaluated_obj with
       | Return_value v -> aux env' v xs
       | _ -> aux env' evaluated_obj xs)
  in
  aux env Null program
;;
