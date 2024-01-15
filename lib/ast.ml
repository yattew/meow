type identifier = string
[@@deriving show]

type program_node = statement_node list
[@@deriving show]

and statement_node =
  | Let_statement of (identifier * expression_node)
  | If_statement
[@@deriving show]

and expression_node = Expression
[@@deriving show]
