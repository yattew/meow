open Meow

let prompt = "=> "

let rec start_repl () =
  print_string prompt;
  flush stdout;
  ()
  |> read_line
  |> Lexer.init
  |> Parser.init
  |> Parser.parse_program
  |> Evaluator.eval_program
  |> Object.string_of_object_type
  |> print_endline;
  start_repl ()
;;

let () = start_repl ()
