module StringMap = Map.Make (String)

type object_type =
  | Integer of int
  | True
  | False
  | Null
  | Function of
      { parameters : Ast.identifier list
      ; body : Ast.block_statement_node
      ; env : env
      }
  | Builtin of (env -> object_type list -> object_type)
  | Return_value of object_type

and table = object_type StringMap.t
and env = table Array.t * int

let env_size = 10000

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

let add (env : env) (name : string) (item : 'a) =
  match env with
  | arr, fill ->
    if fill >= env_size
    then failwith "env filled"
    else arr.(fill) <- StringMap.add name item arr.(fill);
    arr, fill
;;

let extend_env (env1 : env) (env2 : env) =
  match env1, env2 with
  | (arr1, fill1), (arr2, fill2) ->
    if fill1 + fill2 > env_size
    then failwith "cant' extend max env size reached"
    else (
      let rec aux arr1 fill1 arr2 fill2 it =
        if it = fill1
        then arr2, fill2
        else (
          arr2.(fill2 + 1) <- arr1.(it);
          aux arr1 fill1 arr2 fill2 (it + 1))
      in
      aux arr1 fill1 arr2 fill2 0)
;;

let table_get table name =
  match StringMap.find_opt name table with
  | None -> None
  | Some v -> Some v
;;

let rec get (env : env) name =
  match env with
  | arr, fill ->
    if fill == -1
    then None
    else (
      match table_get arr.(fill) name with
      | None -> get (arr, fill - 1) name
      | Some v -> Some v)
;;

let init_env : env = Array.init env_size (fun _ -> StringMap.empty), 0

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
