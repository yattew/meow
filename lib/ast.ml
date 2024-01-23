type operator = string [@@deriving show]
type identifier = string [@@deriving show]
type integer = int [@@deriving show]

type statement_node =
  | Let_statement of (identifier * expression_node)
  | Return_statement of expression_node
  | Expression_statement of expression_node
[@@deriving show]

and expression_node =
  | Expression
  | Identifier_expression of identifier
  | Integer_expression of integer
  | Operator_expression of operator
  | Prefix_expression of operator * expression_node
  | Infix_expression of expression_node * operator * expression_node
[@@deriving show]

type program_node = statement_node list [@@deriving show]
