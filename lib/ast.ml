type operator = string [@@deriving show]
type identifier = string [@@deriving show]
type integer = int [@@deriving show]
type boolean = bool [@@deriving show]

type statement_node =
  | Let_statement of (identifier * expression_node)
  | Return_statement of expression_node
  | Expression_statement of expression_node
[@@deriving show]

and expression_node =
  | Operator_expression of operator
  | Identifier_expression of identifier
  | Integer_expression of integer
  | Boolean_expression of boolean
  | Prefix_expression of operator * expression_node
  | Infix_expression of expression_node * operator * expression_node
[@@deriving show]

type program_node = statement_node list [@@deriving show]

let rec string_of_statement = function
  | Let_statement (id, expr) ->
    Printf.sprintf
      "let %s = %s ;"
      (show_identifier id)
      (string_of_expression expr)
  | Return_statement expr ->
      Printf.sprintf "return %s ;" (string_of_expression expr)
  | Expression_statement expr ->
    Printf.sprintf "%s ;" (string_of_expression expr)

and string_of_expression = function
  | Operator_expression op -> show_operator op
  | Identifier_expression id -> show_identifier id
  | Integer_expression i -> show_integer i
  | Boolean_expression b -> show_boolean b
  | Prefix_expression (op, expression_node) ->
    Printf.sprintf "%s ( %s )" op (string_of_expression expression_node)
  | Infix_expression (l_expr, op, r_expr) ->
    Printf.sprintf
      "( %s %s %s )"
      (string_of_expression l_expr)
      op
      (string_of_expression r_expr)
;;

let rec string_of_program = function
  | [] -> ""
  | x :: xs -> string_of_statement x ^ string_of_program xs
;;
