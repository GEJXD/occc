module T = struct
  include Tokens
end

exception ParserError of string

module Private = struct
  type expected = Tok of T.t | Name of string

  let pp_expected fmt = function
    | Tok token ->
        T.pp fmt token
    | Name s ->
        Format.pp_print_string fmt s

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
    | T.Identifier x ->
        x
    | other ->
        raise_error ~expected:(Name "an identifier") ~actual:other

  let parse_int tokens =
    match Tok_stream.take_token tokens with
    | T.Constant c ->
        Ast.Constant c
    | _ ->
        raise (ParserError "Syntax error")

  (* <unop> ::= "-" | "~" *)
  let parse_unop tokens =
    match Tok_stream.take_token tokens with
    | T.Tilde ->
        Ast.Complement
    | T.Hyphen ->
        Ast.Negate
    | other ->
        raise_error ~expected:(Name "a unary operator") ~actual:other

  (* <exp> ::= <int> | <unop> <exp> | "(" <exp> ")" *)
  let rec parse_exp tokens =
    match Tok_stream.peek tokens with
    | T.Constant _ ->
        parse_int tokens
    | T.Tilde | T.Hyphen ->
        let opera = parse_unop tokens in
        let inner_exp = parse_exp tokens in
        Unary (opera, inner_exp)
    | T.OpenParen ->
        let _ = Tok_stream.take_token tokens in
        let expr = parse_exp tokens in
        expect T.CloseParen tokens ; expr
    | other ->
        raise_error ~expected:(Name "an expression") ~actual:other

  let parse_statement tokens =
    let _ = expect T.KWReturn tokens in
    let return_val = parse_exp tokens in
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
    Ast.Function {name= fun_name; body= statement}

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
  with Tok_stream.End_of_stream -> raise (ParserError "Unexpected end of file")
