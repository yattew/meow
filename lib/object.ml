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
;;

module Env = struct
  type table_entry = string * object_type
  type table = table_entry list

  let add (table : table) (name : string) (item : object_type) =
    let rec aux table name item =
      match table with
      | [] -> [ name, item ]
      | (name', _) :: xs when name' = name -> (name, item) :: xs
      | ((_, _) as x) :: xs -> x :: aux xs name item
    in
    aux table name item
  ;;

  let rec get table name =
    match table with
    | [] -> None
    | x :: xs ->
      (match x with
       | n, i when n = name -> Some i
       | _ -> get xs name)
  ;;

  let init_env = []
end
