type object_type =
  | Integer of int
  | Boolean of bool
  | Null
[@@deriving show]

let string_of_object_type = function
  | Integer i -> string_of_int i
  | Boolean b -> string_of_bool b
  | Null -> "Null"
;;
