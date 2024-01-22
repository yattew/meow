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

type precidence =
  | Lowest
  | Equals
  | Less_greater
  | Sum
  | Product
  | Prefix
  | Call

let get_precidence = function
  | Lowest -> 0
  | Equals -> 1
  | Less_greater -> 2
  | Sum -> 3
  | Product -> 4
  | Prefix -> 5
  | Call -> 6
;;

module ExpressionParser = struct
  let rec after_next_semicolon parser =
    if parser.cur_token.token_type = Token.Semicolon
    then next_token parser, Ast.Expression
    else after_next_semicolon @@ next_token parser
  ;;

  let parse_identifier parser =
    Ast.Identifier_expression parser.cur_token.literal
  ;;

  let parse_integer parser =
    try Ast.Integer_expression (int_of_string parser.cur_token.literal) with
    | Failure _ -> raise @@ Failure "could not parse integer"
  ;;

  let prefix_fn = function
    | Token.Identifier -> parse_identifier
    | Token.Integer -> parse_integer
    | _ -> raise @@ Failure "prefix_fn not implimented"
  ;;

  let parse_expression parser precidence =
    let _ = precidence in
    let prefix = prefix_fn parser.cur_token.token_type in
    let left_exp = prefix parser in
    parser, left_exp
  ;;
end

module StatementParser = struct
  let parse_let_statement parser =
    let parser' = next_token parser in
    let cur_token = parser'.cur_token in
    match cur_token.token_type with
    | Token.Identifier ->
      let parser'', expr_node =
        ExpressionParser.parse_expression parser' Lowest
      in
      parser'', Ast.(Let_statement (cur_token.literal, expr_node))
    | _ -> raise @@ Failure "Expected Identifier"
  ;;

  let parse_return_statement parser =
    let parser' = next_token parser in
    let parser'', expr = ExpressionParser.parse_expression parser' Lowest in
    parser'', Ast.Return_statement expr
  ;;

  let parse_expression_statement parser =
    let parser', expr_node = ExpressionParser.parse_expression parser Lowest in
    let parser'' =
      if parser'.peek_token.token_type = Token.Semicolon
      then next_token @@ next_token parser'
      else parser'
    in
    parser'', Ast.Expression_statement expr_node
  ;;

  let parse_statement parser =
    match parser.cur_token.token_type with
    | Token.Let -> parse_let_statement parser
    | Token.Return -> parse_return_statement parser
    | _ -> parse_expression_statement parser
  ;;
end

let parse_program parser =
  let rec aux parser program =
    if parser.cur_token.token_type = Token.Eof
    then List.rev program
    else (
      let parser', stmt = StatementParser.parse_statement parser in
      aux parser' (stmt :: program))
  in
  aux parser []
;;
