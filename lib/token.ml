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
     | True -> "True"
     | False -> "False"
     | If -> "If"
     | Else -> "Else"
     | Return -> "Return"
     (*operators*)
     | Assign -> "="
     | Plus -> "+"
     | Minus -> "-"
     | Asterics -> "*"
     | Slash -> "/"
     | Lt -> "<"
     | Gt -> ">"
     | Bang -> "!"
     | Eq -> "=="
     | Not_eq -> "!=")
    t.literal
;;

let rec print_tokens = function
  | [] -> ()
  | x :: xs ->
    print_token x;
    print_tokens xs
;;
