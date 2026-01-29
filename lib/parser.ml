module T = struct
  include Tokens
end

exception ParserError of string

module Private = struct
  type expected = Tok of T.t | Name of string

  let pp_expected fmt = function
    | Tok token -> T.pp fmt token
    | Name s -> Format.pp_print_string fmt s

  let raise_error ~expected ~actual =
    let msg =
      Format.asprintf "Expected %a but found %a" pp_expected expected T.pp
        actual
    in
    raise (ParserError msg)

  (* remove next token and verify it's what we expected. if not, raise an
     error *)
  let expect expected tokens =
    let actual = Tok_stream.take_token tokens in
    if actual <> expected then raise_error ~expected:(Tok expected) ~actual
    else ()

  let parse_id tokens =
    match Tok_stream.take_token tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

  let get_precedence = function
    | T.Star | T.Slash | T.Percent -> Some 50
    | T.Plus | T.Hyphen -> Some 45
    | T.LeftShift | T.RightShift -> Some 40
    | T.LessThan | T.LessOrEqual | T.GreaterThan | T.GreaterOrEqual -> Some 35
    | T.DoubleEqual | T.NotEqual -> Some 30
    | T.Ampersand -> Some 29
    | T.Caret -> Some 28
    | T.Pipe -> Some 27
    | T.LogicalAnd -> Some 10
    | T.LogicalOr -> Some 5
    | _ -> None

  let parse_int tokens =
    match Tok_stream.take_token tokens with
    | T.Constant c -> Ast.Constant c
    | other -> raise_error ~expected:(Name "a constant") ~actual:other

  (* <unop> ::= "-" | "~" | "!" *)
  let parse_unop tokens =
    match Tok_stream.take_token tokens with
    | T.Tilde -> Ast.Complement
    | T.Hyphen -> Ast.Negate
    | T.Bang -> Ast.Not
    | other -> raise_error ~expected:(Name "a unary operator") ~actual:other

  (* <binop> ::= "-" | "+" | "*" | "/" | "%" | "&&" | "||" | "==" | "!=" | "<" |
     "<=" | ">" | ">=" *)
  let parse_binop tokens =
    match Tok_stream.take_token tokens with
    | T.Plus -> Ast.Add
    | T.Hyphen -> Ast.Subtract
    | T.Star -> Ast.Multiply
    | T.Slash -> Ast.Divide
    | T.Percent -> Ast.Mod
    | T.Pipe -> Ast.BitOr
    | T.Caret -> Ast.BitXor
    | T.Ampersand -> Ast.BitAnd
    | T.LeftShift -> Ast.ShiftLeft
    | T.RightShift -> Ast.ShiftRight
    | T.LogicalAnd -> Ast.And
    | T.LogicalOr -> Ast.Or
    | T.DoubleEqual -> Ast.Equal
    | T.NotEqual -> Ast.NotEqual
    | T.LessThan -> Ast.LessThan
    | T.LessOrEqual -> Ast.LessOrEqual
    | T.GreaterThan -> Ast.GreaterThan
    | T.GreaterOrEqual -> Ast.GreaterOrEqual
    | other -> raise_error ~expected:(Name "a binary operator") ~actual:other

  (* <factor> ::= <int> | <unop> <factor> | "(" <exp> ")" *)
  let rec parse_factor tokens =
    match Tok_stream.peek tokens with
    | T.Constant _ -> parse_int tokens
    | T.Tilde | T.Hyphen | T.Bang ->
        let opera = parse_unop tokens in
        let inner_exp = parse_factor tokens in
        Ast.Unary (opera, inner_exp)
    | T.OpenParen ->
        let _ = Tok_stream.take_token tokens in
        let expr = parse_exp 0 tokens in
        (* condition like -(2 + 3) *)
        expect T.CloseParen tokens;
        expr
    | other -> raise_error ~expected:(Name "a factor") ~actual:other

  and
      (* <exp> ::= <factor> | <exp> <binop> <exp> *)
      parse_exp min_prec tokens =
    (* like acc in tail recursive, initial value of left-associative *)
    let initial_factor = parse_factor tokens in
    let next_token = Tok_stream.peek tokens in
    let rec parse_exp_loop left token =
      match get_precedence token with
      (* 1 + (2 * 3) => the right subexpression must have higher precedence than
         left *)
      | Some prec when prec >= min_prec ->
          let operator = parse_binop tokens in
          let right = parse_exp (prec + 1) tokens in
          let result = Ast.Binary (operator, left, right) in
          parse_exp_loop result (Tok_stream.peek tokens)
      (* otherwise, just left-associative *)
      | _ -> left
    in
    parse_exp_loop initial_factor next_token

  let parse_statement tokens =
    let _ = expect T.KWReturn tokens in
    let return_val = parse_exp 0 tokens in
    let _ = expect T.Semicolon tokens in
    Ast.Return return_val

  let parse_function_definition tokens =
    let _ = expect T.KWInt tokens in
    let fun_name = parse_id tokens in
    let _ = expect T.OpenParen tokens in
    let _ = expect T.KWVoid tokens in
    let _ = expect T.CloseParen tokens in
    let _ = expect T.OpenBrace tokens in
    let statement = parse_statement tokens in
    let _ = expect T.CloseBrace tokens in
    Ast.Function { name = fun_name; body = statement }

  let parse_program tokens =
    let fun_def = parse_function_definition tokens in
    if Tok_stream.is_empty tokens then Ast.Program fun_def
    else raise (ParserError "Unexpected tokens after function definition")
end

let parser tokens =
  try
    let token_stream = Tok_stream.of_list tokens in
    let ast = Private.parse_program token_stream in
    let _ = Ast.PrintAst.print_program ast in
    ast
  with Tok_stream.End_of_stream ->
    raise (ParserError "Unexpected end of file")
