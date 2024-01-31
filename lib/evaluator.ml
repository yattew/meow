let eval_expression node =
  let open Ast in
  match node with
  | Integer_expression i -> Object.Integer i
  | Boolean_expression b -> Object.Boolean b
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
