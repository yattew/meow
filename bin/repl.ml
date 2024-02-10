open Meow

let prompt = "=> "

let rec start_repl env =
  print_string prompt;
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

let () = start_repl Object.Env.init_env
