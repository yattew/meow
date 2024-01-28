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

type precedence =
  | Precedence_lowest
  | Precedence_equals
  | Precedence_less_greater
  | Precedence_sum
  | Precedence_product
  | Precedence_prefix
  | Precedence_call

let int_of_precedence = function
  | Precedence_lowest -> 0
  | Precedence_equals -> 1
  | Precedence_less_greater -> 2
  | Precedence_sum -> 3
  | Precedence_product -> 4
  | Precedence_prefix -> 5
  | Precedence_call -> 6
;;

let precedence_of_token tt =
  let open Token in
  match tt.token_type with
  | Plus -> Precedence_sum
  | Minus -> Precedence_sum
  | Asterics -> Precedence_product
  | Slash -> Precedence_product
  | Lt -> Precedence_less_greater
  | Gt -> Precedence_less_greater
  | Eq -> Precedence_equals
  | Not_eq -> Precedence_equals
  | Lparen -> Precedence_call
  | _ -> Precedence_lowest
;;

let expect_peek parser t =
  let open Token in
  if parser.peek_token.token_type != t
  then
    raise
    @@ Failure
         (Printf.sprintf
            "Expected peek token %s found %s at %d"
            (Token.show_token_type t)
            (Token.show_token_type parser.peek_token.token_type)
            parser.lexer.position)
  else ()
;;

let expect_cur parser t =
  let open Token in
  if parser.cur_token.token_type != t
  then
    raise
    @@ Failure
         (Printf.sprintf
            "Expected cur token %s found %s at %d"
            (Token.show_token_type t)
            (Token.show_token_type parser.peek_token.token_type)
            parser.lexer.position)
  else ()
;;

module rec ExpressionParser : sig
  val parse_expression : parser -> precedence -> parser * Ast.expression_node
end = struct
  let parse_integer parser =
    try
      parser, Ast.Integer_expression (int_of_string parser.cur_token.literal)
    with
    | Failure _ -> raise @@ Failure "could not parse integer"
  ;;

  let rec parse_prefix_expression parser =
    let parser'', right_expr =
      parse_expression (next_token parser) Precedence_prefix
    in
    parser'', Ast.Prefix_expression (parser.cur_token.literal, right_expr)

  and parse_grouped_expression parser =
    let parser', expr =
      parse_expression (next_token parser) Precedence_lowest
    in
    expect_peek parser' Token.Rparen;
    next_token parser', expr

  and parse_if_expression parser =
    expect_peek parser Token.Lparen;
    let parser', condition =
      parse_expression (next_token parser) Precedence_lowest
    in
    expect_cur parser' Token.Rparen;
    expect_peek parser' Token.Lbrace;
    let parser'', consequence =
      StatementParser.parse_block_statement (next_token parser')
    in
    let parser''', alternative =
      match parser''.cur_token.token_type with
      | Token.Else ->
        let parser''' = next_token parser'' in
        expect_cur parser''' Token.Lbrace;
        StatementParser.parse_block_statement parser'''
      | _ -> parser'', []
    in
    parser''', Ast.If_expression { condition; consequence; alternative }

  and parse_function_params parser =
    let open Token in
    let parser = next_token parser in
    if parser.cur_token.token_type = Rparen
    then parser, []
    else (
      let rec iter_params parser param_list =
        if parser.peek_token.token_type != Comma
        then parser, parser.cur_token.literal :: param_list |> List.rev
        else
          iter_params
            (next_token @@ next_token parser)
            (parser.cur_token.literal :: param_list)
      in
      let parser'', param_list = iter_params parser [] in
      expect_peek parser'' Rparen;
      next_token parser'', param_list)

  and parse_function_expression parser =
    expect_peek parser Token.Lparen;
    let parser', parameters = parse_function_params (next_token parser) in
    expect_peek parser' Token.Lbrace;
    let parser'', body =
      StatementParser.parse_block_statement (next_token parser')
    in
    parser'', Ast.Fn_expression { parameters; body }

  and parse_infix_expression parser left =
    let precedence = precedence_of_token parser.cur_token in
    let parser'', right = parse_expression (next_token parser) precedence in
    parser'', Ast.Infix_expression (left, parser.cur_token.literal, right)

  and parse_call_arguments parser =
    let open Token in
    let parser = next_token parser in
    if parser.cur_token.token_type = Rparen
    then next_token parser, []
    else (
      let rec iter_params parser exp_list =
        let parser', exp = parse_expression parser Precedence_lowest in
        if parser'.peek_token.token_type != Comma
        then parser', exp :: exp_list |> List.rev
        else iter_params (next_token @@ next_token parser') (exp :: exp_list)
      in
      let parser'', param_list = iter_params parser [] in
      print_endline @@ show_parser parser'';
      expect_peek parser'' Rparen;
      next_token parser'', param_list)

  and parse_call_expression parser left =
    let parser', args = parse_call_arguments parser in
    ( next_token @@ next_token parser'
    , Ast.Call_expression { func_exp = left; args } )

  and prefix_fn t =
    let open Token in
    let open Ast in
    match t.token_type with
    | Identifier -> fun p -> p, Identifier_expression p.cur_token.literal
    | Integer -> parse_integer
    | Bang -> parse_prefix_expression
    | Minus -> parse_prefix_expression
    | True -> fun p -> p, Boolean_expression true
    | False -> fun p -> p, Boolean_expression false
    | Lparen -> parse_grouped_expression
    | If -> parse_if_expression
    | Function -> parse_function_expression
    | _ ->
      raise
      @@ Failure
           (Printf.sprintf
              "prefix_fn not implimented for type %s"
              (Token.show_token t))

  and infix_of_parser t =
    let open Token in
    match t.token_type with
    | Plus -> Some parse_infix_expression
    | Minus -> Some parse_infix_expression
    | Asterics -> Some parse_infix_expression
    | Slash -> Some parse_infix_expression
    | Lt -> Some parse_infix_expression
    | Gt -> Some parse_infix_expression
    | Eq -> Some parse_infix_expression
    | Not_eq -> Some parse_infix_expression
    | Lparen -> Some parse_call_expression
    | _ -> None

  and parse_expression parser precedence =
    let rec aux parser left_exp =
      let open Token in
      if parser.peek_token.token_type = Semicolon
         || int_of_precedence precedence
            >= int_of_precedence @@ precedence_of_token parser.peek_token
      then parser, left_exp
      else (
        match infix_of_parser parser.peek_token with
        | None -> aux (next_token parser) left_exp
        | Some f ->
          let parser', left_exp' = f (next_token parser) left_exp in
          aux parser' left_exp')
    in
    let parser', left_exp = prefix_fn parser.cur_token parser in
    aux parser' left_exp
  ;;
end

and StatementParser : sig
  val parse_block_statement : parser -> parser * Ast.block_statement_node
  val parse_statement : parser -> parser * Ast.statement_node
end = struct
  let parse_let_statement parser =
    expect_peek parser Token.Identifier;
    let parser' = next_token parser in
    let cur_token = parser'.cur_token in
    let parser'', expr_node =
      ExpressionParser.parse_expression
        (next_token @@ next_token parser')
        Precedence_lowest
    in
    ( next_token @@ next_token parser''
    , Ast.Let_statement (cur_token.literal, expr_node) )
  ;;

  let parse_return_statement parser =
    let parser' = next_token parser in
    let parser'', expr =
      ExpressionParser.parse_expression parser' Precedence_lowest
    in
    next_token @@ next_token parser'', Ast.Return_statement expr
  ;;

  let parse_expression_statement parser =
    let parser', expr_node =
      ExpressionParser.parse_expression parser Precedence_lowest
    in
    next_token @@ next_token parser', Ast.Expression_statement expr_node
  ;;

  let rec parse_block_statement parser =
    let parser' = next_token parser in
    let rec aux parser block =
      if parser.cur_token.token_type != Token.Rbrace
         && parser.cur_token.token_type != Token.Eof
      then (
        let parser', stmt = parse_statement parser in
        aux parser' (stmt :: block))
      else parser, List.rev block
    in
    aux parser' []

  and parse_statement parser =
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
