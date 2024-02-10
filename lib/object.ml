module Env = struct
  type 'a table_entry = string * 'a
  type 'a table = 'a table_entry list
  type 'a env = 'a table list

  let pp_table_entry show_val entry =
    match entry with
    | s, item -> Printf.sprintf "(%s,%s)" s (show_val item)
  ;;

  let pp_table show_val table =
    table
    |> List.map @@ pp_table_entry show_val
    |> String.concat ", "
    |> Printf.sprintf "[ %s ]"
  ;;

  let pp_env show_val env =
    env
    |> List.map @@ pp_table show_val
    |> String.concat ", "
    |> Printf.sprintf "[ %s ]"
  ;;

  let add (tables : 'a env) (name : string) (item : 'a) =
    let rec aux table name item =
      match table with
      | [] -> [ name, item ]
      | (name', _) :: xs when name' = name -> (name, item) :: xs
      | ((_, _) as x) :: xs -> x :: aux xs name item
    in
    match tables with
    | [] -> [ [ name, item ] ]
    | table :: rest -> aux table name item :: rest
  ;;

  let extend_env env1 env2 = env1 @ env2

  let rec table_get table name =
    match table with
    | [] -> None
    | x :: xs ->
      (match x with
       | n, i when n = name -> Some i
       | _ -> table_get xs name)
  ;;

  let rec get tables name =
    match tables with
    | [] -> None
    | table :: xs ->
      (match table_get table name with
       | None -> get xs name
       | Some v -> Some v)
  ;;

  let init_env = []
end

type object_type =
  | Integer of int
  | True
  | False
  | Null
  | Function of
      { parameters : Ast.identifier list
      ; body : Ast.block_statement_node
      ; env : object_type Env.env
      }
  | Builtin of (object_type Env.env -> object_type list -> object_type)
  | Return_value of object_type

let rec string_of_object_type = function
  | Integer i -> string_of_int i
  | True -> "true"
  | False -> "false"
  | Null -> "Null"
  | Function { parameters; _ } ->
    Printf.sprintf "fn (%s)" (parameters |> String.concat ",")
  | Builtin _ -> "builtin"
  | Return_value obj -> string_of_object_type obj
;;

let object_type_of_bool = function
  | true -> True
  | false -> False
;;
