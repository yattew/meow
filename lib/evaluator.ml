open Ast
open Object

let is_truthy obj =
  match obj with
  | Integer i -> if i = 0 then false else true
  | True -> true
  | False -> false
  | Null -> false
  | Return_value _ -> raise (Failure "cannot use operator on a return")
;;

let eval_bang_operator_expression _ = function
  | Integer i when i = 0 -> True
  | Integer _ -> False
  | True -> False
  | False -> True
  | Null -> True
  | Return_value _ -> raise (Failure "cannot use ! operator on a return")
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
  | _ -> Null
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

and eval_expression env node =
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
    (match Env.get env id with
     | Some obj -> obj
     | None ->
       raise
         (Failure (Printf.sprintf "Identifier %s not found in enviornment" id)))
  | _ -> Null

and eval_statement env = function
  | Let_statement (id, exp) -> eval_expression env exp |> Env.add env id, Null
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

and eval_program env = function
  | [] -> env, Null
  | xs -> eval_statements env xs
;;
