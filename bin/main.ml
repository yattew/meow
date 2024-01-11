let sample_code =
  "let five = 5;\n\
   let ten = 10;\n\
   let add = fn(x, y) {\n\
   x + y;\n\
   };\n\
   let result = add(five, ten);\n\
   !-/*5;\n\
   5 < 10 > 5;"
;;

let rec text_lexer (l : Meow.Lexer.lexer) =
  let l', t = Meow.Lexer.next_token l in
  Meow.Token.print_token t;
  if t.token_type = Meow.Token.Illegal
  then
    Printf.printf
      "%s, char: %c with code: %d"
      "Illegal token found"
      l.ch
      (Char.code l.ch)
  else if t.token_type = Meow.Token.Eof
  then print_endline "Eof token reached"
  else text_lexer l'
;;

let () =
  print_endline "code: ";
  print_endline sample_code;
  text_lexer @@ Meow.Lexer.init sample_code
;;
