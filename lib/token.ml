type token_type =
  | Illegal
  | Eof
  | Identifier of string
  | Integer of string
  | Assign
  | Plus
  | Comma
  | Semicolon
  | Lparan
  | Rparan
  | Lbrace
  | Rbrace
  (*keywords*)
  | Function
  | Let


type token =
  { token_type : token_type
  ; literal : string
  }

let print_token t =
  Printf.printf "{ token_type = %s; literal = %s }\n"
    (match t.token_type with
    | Illegal -> "Illegal"
    | Eof -> "Eof"
    | Identifier s -> Printf.sprintf "Identifier \"%s\"" s
    | Integer i -> i
    | Assign -> "Assign"
    | Plus -> "Plus"
    | Comma -> "Comma"
    | Semicolon -> "Semicolon"
    | Lparan -> "Lparan"
    | Rparan -> "Rparan"
    | Lbrace -> "Lbrace"
    | Rbrace -> "Rbrace"
    | Function -> "Function"
    | Let -> "Let")
    t.literal
