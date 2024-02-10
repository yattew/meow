let rec print_endl env args =
  match args with
  | [] -> Object.Null
  | x :: xs ->
    print_endline @@ Object.string_of_object_type x;
    print_endl env xs
;;
