type object_type =
  | Integer of int
  | True
  | False
  | Null
[@@deriving show]

let string_of_object_type = function
  | Integer i -> string_of_int i
  | True -> "true"
  | False -> "false"
  | Null -> "Null"
;;

let object_type_of_bool = function
  | true -> True
  | false -> False
