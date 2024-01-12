let prompt = "=> "

let extract_tokens lexer =
  let rec aux lexer acc =
    let lexer', token = Meow.Lexer.next_token lexer in
    if token.token_type = Meow.Token.Eof
    then List.rev acc
    else aux lexer' (token :: acc)
  in
  aux lexer []
;;

let rec start_repl () =
  print_string prompt;
  flush stdout;
  let input = read_line () in
  let lexer = Meow.Lexer.init input in
  lexer |> extract_tokens |> Meow.Token.print_tokens;
  start_repl ()
;;

let () = start_repl ()
