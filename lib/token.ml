type token_type =
  | Illegal
  | Eof
  | Identifier of string
  | Integer of string
  | Comma
  | Semicolon
  | Lparan
  | Rparan
  | Lbrace
  | Rbrace
  (*keywords*)
  | Function
  | Let
  (*operators*)
  | Assign
  | Plus
  | Minus
  | Asterics
  | Slash
  | Lt
  | Gt
  | Bang

type token =
  { token_type : token_type
  ; literal : string
  }

let print_token t =
  Printf.printf
    "{ token_type = %s; literal = %s }\n"
    (match t.token_type with
     | Illegal -> "Illegal"
     | Eof -> "Eof"
     | Identifier s -> Printf.sprintf "Identifier \"%s\"" s
     | Integer i -> i
     | Comma -> ","
     | Semicolon -> ";"
     | Lparan -> "("
     | Rparan -> ")"
     | Lbrace -> "{"
     | Rbrace -> "}"
     (*keywords*)
     | Function -> "Function"
     | Let -> "Let"
     (*operators*)
     | Assign -> "="
     | Plus -> "+"
     | Minus -> "-"
     | Asterics -> "*"
     | Slash -> "/"
     | Lt -> "<"
     | Gt -> ">"
     | Bang -> "!")
    t.literal
;;
