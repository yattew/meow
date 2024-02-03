open Ast
open Object

let is_truthy obj =
  match obj with
  | Integer i -> if i = 0 then false else true
  | True -> true
  | False -> false
  | Null -> false
;;

let eval_bang_operator_expression = function
  | Integer i when i = 0 -> True
  | Integer _ -> False
  | True -> False
  | False -> True
  | Null -> True
;;

let eval_minus_operator_expression = function
  | Integer i -> Integer (-i)
  | x ->
    raise
      (Failure
         (Printf.sprintf
            "cannot use - operator with %s"
            (string_of_object_type x)))
;;

let eval_prefix_expression op right =
  match op with
  | "!" -> eval_bang_operator_expression right
  | "-" -> eval_minus_operator_expression right
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

let rec eval_if_expression = function
  | If_expression { condition; consequence; alternative } ->
    if is_truthy @@ eval_expression condition
    then eval_block_statement consequence
    else eval_block_statement alternative
  | _ -> raise (Failure "can't be reached")

and eval_expression node =
  let open Ast in
  match node with
  | Integer_expression i -> Integer i
  | Boolean_expression b -> if b then True else False
  | Prefix_expression (op, expr) ->
    eval_expression expr |> eval_prefix_expression op
  | Infix_expression (l_expr, op, r_expr) ->
    let l_evaluated = eval_expression l_expr in
    let r_evaluated = eval_expression r_expr in
    eval_infix_expression l_evaluated op r_evaluated
  | If_expression expr -> eval_if_expression (If_expression expr)
  | _ -> Null

and eval_statement = function
  | Ast.Let_statement _ -> Null
  | Ast.Return_statement _ -> Null
  | Ast.Expression_statement expr -> eval_expression expr

and eval_statements stmts =
  List.fold_left (fun _ i -> eval_statement i) Null stmts

and eval_block_statement stmts = eval_statements stmts

and eval_program = function
  | [] -> Null
  | xs -> eval_statements xs
;;
