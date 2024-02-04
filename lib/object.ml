type object_type =
  | Integer of int
  | True
  | False
  | Null
  | Return_value of object_type
[@@deriving show]

let rec string_of_object_type = function
  | Integer i -> string_of_int i
  | True -> "true"
  | False -> "false"
  | Null -> "Null"
  | Return_value obj -> string_of_object_type obj
;;

let object_type_of_bool = function
  | true -> True
  | false -> False
