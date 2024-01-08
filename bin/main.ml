let sample_code =
  "let five = 5;\n\
   let ten = 10;\n\
   let add = fn(x, y) {\n\
   x + y;\n\
   };\n\
   let result = add(five, ten);"
;;
(* this is just to ensure there are no compiler errors*)
let _ =
  let open Meow in
  Lexer.init "+"
;;

let () =
  print_string "sample code for lexing:";
  print_string sample_code
;;
