type token_type =
  | Illegal
  | Eof
  | Identifier
  | Integer
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  (*keywords*)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
  (*operators*)
  | Assign
  | Plus
  | Minus
  | Asterics
  | Slash
  | Lt
  | Gt
  | Bang
  | Eq
  | Not_eq
[@@deriving show]

type token =
  { token_type : token_type
  ; literal : string
  }
[@@deriving show]

let rec print_tokens = function
  | [] -> ()
  | x :: xs ->
    print_endline @@ show_token x;
    print_tokens xs
;;
