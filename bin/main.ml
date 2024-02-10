open Meow

let main_env =
  let open Object in
  Env.add Env.init_env "print_endl" (Object.Builtin Builtins.print_endl)
;;

let rec start_repl env =
  print_string "=> ";
  flush stdout;
  (try
     let env, res =
       ()
       |> read_line
       |> Lexer.init
       |> Parser.init
       |> Parser.parse_program
       |> Evaluator.eval_program env
     in
     Object.string_of_object_type res |> print_endline;
     (*
        print_endline @@ Object.Env.pp_env Object.string_of_object_type env;
     *)
     start_repl env
   with
   | Failure msg -> print_endline msg);
  start_repl env
;;

let read_file filename =
  try
    let channel = open_in filename in
    try
      let rec read_lines acc =
        try
          let line = input_line channel in
          read_lines (line :: acc)
        with
        | End_of_file -> List.rev acc
      in
      let lines = read_lines [] in
      close_in channel;
      lines
    with
    | ex ->
      close_in channel;
      raise ex
  with
  | Sys_error msg ->
    Printf.eprintf "Error: %s\n" msg;
    []
;;

let () =
  if Array.length Sys.argv > 1
  then (
    let file_name = Sys.argv.(1) in
    let lines = read_file file_name in
    let text = String.concat "\n" lines in
    let _, _=
      text
      |> Lexer.init
      |> Parser.init
      |> Parser.parse_program
      |> Evaluator.eval_program main_env
    in
    ())
  else start_repl main_env
;;
