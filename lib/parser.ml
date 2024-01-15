type parser =
  { lexer : Lexer.lexer
  ; cur_token : Token.token
  ; peek_token : Token.token
  }
[@@deriving show]

let next_token p =
  let l', t' = Lexer.next_token p.lexer in
  { lexer = l'; cur_token = p.peek_token; peek_token = t' }
;;

let init l =
  next_token
  @@ next_token
       { lexer = l
       ; cur_token = Utils.zero_token
       ; peek_token = Utils.zero_token
       }
;;

let parse_expression parser =
  let rec aux parser =
    if parser.cur_token.token_type = Token.Eof
    then parser, Ast.Expression
    else aux @@ next_token parser
  in
  aux parser
;;

let parse_let_statement parser =
  let parser' = next_token parser in
  let cur_token = parser'.cur_token in
  print_endline @@ Token.show_token cur_token;
  match cur_token.token_type with
  | Token.Identifier ->
    let parser'', expr_node = parse_expression parser' in
    parser'', Ast.(Let_statement (cur_token.literal, expr_node))
  | _ -> raise @@ Failure "Expected Identifier"
;;

let parse_statement parser =
  print_endline @@ Token.show_token parser.cur_token;
  match parser.cur_token.token_type with
  | Token.Let -> parse_let_statement parser
  | _ -> raise @@ Failure "Expected let statement"
;;

let parse_program parser =
  let rec aux parser program =
    if parser.cur_token.token_type = Token.Eof
    then List.rev program
    else (
      let parser', stmt = parse_statement parser in
      aux parser' (stmt :: program))
  in
  aux parser []
;;
