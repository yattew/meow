let eval_bang_operator_expression = function
  | Object.Integer i when i = 0 -> Object.True
  | Object.Integer _ -> Object.False
  | Object.True -> Object.False
  | Object.False -> Object.True
  | Object.Null -> Object.True
;;

let eval_minus_operator_expression = function
  | Object.Integer i -> Object.Integer (-i)
  | x ->
    raise
      (Failure
         (Printf.sprintf
            "cannot use - operator with %s"
            (Object.string_of_object_type x)))
;;

let eval_prefix_expression op right =
  match op with
  | "!" -> eval_bang_operator_expression right
  | "-" -> eval_minus_operator_expression right
  | _ -> Object.Null
;;

let rec eval_expression node =
  let open Ast in
  match node with
  | Integer_expression i -> Object.Integer i
  | Boolean_expression b -> if b then Object.True else Object.False
  | Prefix_expression (op, expr) ->
    eval_expression expr |> eval_prefix_expression op
  | _ -> Object.Null
;;

let eval_statement = function
  | Ast.Let_statement _ -> Object.Null
  | Ast.Return_statement _ -> Object.Null
  | Ast.Expression_statement expr -> eval_expression expr
;;

let eval_statements = List.fold_left (fun _ i -> eval_statement i) Object.Null

let eval_program = function
  | [] -> Object.Null
  | xs -> eval_statements xs
;;
