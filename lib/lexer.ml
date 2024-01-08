type token_type =
  | Illegal
  | Eof
  | Identifier of string
  | Integer
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

type lexer =
  { input : string
  ; position : int
  ; read_position : int
  ; ch : char
  }

let zero_char = char_of_int 0

let read_char { input; read_position; _ } =
  if read_position >= String.length input
  then
    { input
    ; position = read_position
    ; read_position = read_position + 1
    ; ch = zero_char
    }
  else
    { input
    ; position = read_position
    ; read_position = read_position + 1
    ; ch = String.get input read_position
    }
;;

let is_letter c =
  let ascii_value = Char.code c in
  (ascii_value >= Char.code 'A' && ascii_value <= Char.code 'Z')
  || (ascii_value >= Char.code 'a' && ascii_value <= Char.code 'z')
  || c = '_'
;;

let read_identifier lexer =
  let rec aux lexer acc =
    if is_letter lexer.ch
    then aux (read_char lexer) (lexer.ch :: acc)
    else lexer, acc |> List.rev |> Utils.string_of_char_list
  in
  aux lexer []
;;

let lookup_ident = function
  | "fn" -> Function
  | "let" -> Let
  | x -> Identifier x
;;

let rec skip_whitespace l = 
  if l.ch = ' ' || l.ch = '\t' || l.ch = '\r' then skip_whitespace @@ read_char l
  else l

let next_token lexer =
  let { ch; _ } = lexer in
  match ch with
  | '=' -> read_char lexer, { token_type = Assign; literal = String.make 1 ch }
  | ';' ->
    read_char lexer, { token_type = Semicolon; literal = String.make 1 ch }
  | '(' -> read_char lexer, { token_type = Lparan; literal = String.make 1 ch }
  | ')' -> read_char lexer, { token_type = Rparan; literal = String.make 1 ch }
  | ',' -> read_char lexer, { token_type = Comma; literal = String.make 1 ch }
  | '+' -> read_char lexer, { token_type = Plus; literal = String.make 1 ch }
  | '{' -> read_char lexer, { token_type = Lbrace; literal = String.make 1 ch }
  | '}' -> read_char lexer, { token_type = Rbrace; literal = String.make 1 ch }
  | x when is_letter x ->
    let lexer, literal = read_identifier lexer in
    lexer, { token_type = lookup_ident literal; literal = String.make 1 ch }
  | _ ->
    read_char lexer, { token_type = Illegal; literal = String.make 1 zero_char }
;;

let init input =
  read_char { input; position = 0; read_position = 0; ch = zero_char }
;;
