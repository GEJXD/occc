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

  (* <identifier> ::= ? An identifier token ? *)
  let parse_id tokens =
    match Tok_stream.take_token tokens with
    | T.Identifier x -> x
    | other -> raise_error ~expected:(Name "an identifier") ~actual:other

  (* check whether a token is a specifier *)
  let is_specifier = function
    | T.KWStatic | T.KWExtern | T.KWInt -> true
    | _ -> false

  (* <specifier> ::= "int" | "static" | "extern" *)
  let parse_specifier tokens =
    let spec = Tok_stream.take_token tokens in
    if is_specifier spec then spec
    else
      raise_error ~expected:(Name "a type or storage-class specifier")
        ~actual:spec

  let rec parse_specifier_list tokens =
    let spec = parse_specifier tokens in
    if is_specifier (Tok_stream.peek tokens) then
      spec :: parse_specifier_list tokens
    else [ spec ]

  let parse_storage_class = function
    | T.KWExtern -> Ast.Extern
    | T.KWStatic -> Ast.Static
    | other ->
        raise_error ~expected:(Name "a storage class specifier") ~actual:other

  let parse_type_and_sotrage_class specifier_list =
    let types, storage_classes =
      List.partition (fun tok -> tok = T.KWInt) specifier_list
    in
    if List.length types <> 1 then raise (ParserError "Invalid type specifier")
    else
      let storage_class =
        match storage_classes with
        | [] -> None
        | [ sc ] -> Some (parse_storage_class sc)
        | _ :: _ -> raise (ParserError "Invalid storage class")
      in
      (Types.Int, storage_class)

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
    | T.QuestionMark -> Some 3
    | T.EqualSign | T.PlusEqual | T.HyphenEqual | T.StarEqual | T.SlashEqual
    | T.PercentEqual | T.AmpersandEqual | T.PipeEqual | T.CaretEqual
    | T.LeftShiftEqual | T.RightShiftEqual ->
        Some 1
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
     "<=" | ">" | ">=" | "=" *)
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

  (* <comop> ::= "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" |
     ">>=" *)
  let parse_comop tokens =
    match Tok_stream.take_token tokens with
    | T.PlusEqual -> Ast.AddAssign
    | T.HyphenEqual -> Ast.SubtractAssign
    | T.StarEqual -> Ast.MultiplyAssign
    | T.SlashEqual -> Ast.DivideAssign
    | T.PercentEqual -> Ast.ModAssign
    | T.AmpersandEqual -> Ast.BitAndAssign
    | T.PipeEqual -> Ast.BitOrAssign
    | T.CaretEqual -> Ast.BitXorAssign
    | T.LeftShiftEqual -> Ast.ShiftLeftAssign
    | T.RightShiftEqual -> Ast.ShiftRightAssign
    | other -> raise_error ~expected:(Name "a compound operator") ~actual:other

  let is_compound_assignment = function
    | T.PlusEqual | T.HyphenEqual | T.StarEqual | T.SlashEqual | T.PercentEqual
    | T.AmpersandEqual | T.PipeEqual | T.CaretEqual | T.LeftShiftEqual
    | T.RightShiftEqual ->
        true
    | _ -> false

  (* <factor> ::= | <int> | <identifier> | <unop> <factor> | | "(" <exp> ")" |
     <identifier> "(" [ <argument-list> ] ")"*)
  let rec parse_factor tokens =
    match Tok_stream.peek tokens with
    | T.Constant _ -> parse_int tokens
    | T.Identifier _ ->
        let id = parse_id tokens in
        (* a function call *)
        if Tok_stream.peek tokens = T.OpenParen then
          let _ = Tok_stream.take_token tokens in
          let args =
            (* have no argument *)
            if Tok_stream.peek tokens = T.CloseParen then []
            else parse_argument_list tokens
          in
          let _ = expect T.CloseParen tokens in
          Ast.FunCall { f = id; args }
        else Ast.Var id
    (* Unary expressions *)
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

  and parse_argument_list tokens =
    let arg = parse_exp 0 tokens in
    if Tok_stream.peek tokens = T.Comma then
      let _ = Tok_stream.take_token tokens in
      arg :: parse_argument_list tokens
    else [ arg ]

  and parse_conditional_middle tokens =
    let _ = expect T.QuestionMark tokens in
    let exp = parse_exp 0 tokens in
    let _ = expect T.Colon tokens in
    exp

  and parse_exp min_prec tokens =
    let initial_factor = parse_factor tokens in
    let next_token = Tok_stream.peek tokens in
    let rec parse_exp_loop left next =
      match get_precedence next with
      | Some prec when prec >= min_prec ->
          let result =
            (* right association *)
            if next = T.EqualSign then
              let _ = Tok_stream.take_token tokens in
              let right = parse_exp prec tokens in
              Ast.Assignment (left, right) (* right association *)
            else if next = T.QuestionMark then
              let middle = parse_conditional_middle tokens in
              let right = parse_exp prec tokens in
              Ast.Conditional
                { condition = left; then_result = middle; else_result = right }
              (* right association *)
            else if is_compound_assignment next then
              let operator = parse_comop tokens in
              let right = parse_exp prec tokens in
              Ast.CompoundAssign (operator, left, right)
            (* left association *)
              else
              let operator = parse_binop tokens in
              let right = parse_exp (prec + 1) tokens in
              Ast.Binary (operator, left, right)
          in
          parse_exp_loop result (Tok_stream.peek tokens)
      | _ -> left
    in
    parse_exp_loop initial_factor next_token

  let parse_optional_exp delimit tokens =
    if Tok_stream.peek tokens = delimit then
      let _ = Tok_stream.take_token tokens in
      None
    else
      let e = parse_exp 0 tokens in
      let _ = expect delimit tokens in
      Some e

  (* <param-list> ::= "void" | "int" <identifier> {"," "int" <identifier> } *)
  let parse_param_list tokens =
    if Tok_stream.peek tokens = T.KWVoid then
      let _ = Tok_stream.take_token tokens in
      []
    else
      let rec param_loop () =
        let _ = expect T.KWInt tokens in
        let next_param = parse_id tokens in
        if Tok_stream.peek tokens = T.Comma then
          let _ = Tok_stream.take_token tokens in
          next_param :: param_loop ()
        else [ next_param ]
      in
      param_loop ()

  (* function-declaration> ::= { <specifier> } <identifier> 
     "(" <param-list> ")" ( <block> | ";" ) *)
  (* <variable-declaration> ::= { <specifier> } <identifier> [ "=" <exp> ] ";" *)
  (* parse function and variable are pretty same, we combine them 
      as a single function  *)
  let rec parse_declaration tokens =
    let specifiers = parse_specifier_list tokens in
    let _type, storage_class = parse_type_and_sotrage_class specifiers in
    let name = parse_id tokens in
    match Tok_stream.peek tokens with
    (* a function declaration *)
    | T.OpenParen ->
        let _ = Tok_stream.take_token tokens in
        let params = parse_param_list tokens in
        let _ = expect T.CloseParen tokens in
        let body =
          match Tok_stream.peek tokens with
          | T.Semicolon ->
              let _ = Tok_stream.take_token tokens in
              None
          | _ -> Some (parse_block tokens)
        in
        Ast.FunDecl { name; storage_class; params; body }
    (* a variable declaration *)
    | tok ->
        let init =
          if tok = T.EqualSign then
            let _ = Tok_stream.take_token tokens in
            Some (parse_exp 0 tokens)
          else None
        in
        let _ = expect T.Semicolon tokens in
        Ast.VarDecl { name; storage_class; init }

  (* <for-init> ::= <variable-declaration> | [ <exp> ] ";" *)
  and parse_for_init tokens =
    if is_specifier (Tok_stream.peek tokens) then
      match parse_declaration tokens with
      | Ast.VarDecl vd -> Ast.InitDecl vd
      | _ ->
          raise
            (ParserError
               "Found a function declaration in a for loop initialzer.")
    else
      let opt_e = parse_optional_exp T.Semicolon tokens in
      Ast.InitExp opt_e

  (* <statement> ::= "return" <exp> ";"
   *               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
   *               | <block>
   *               | "break" ";"
   *               | "continue" ";"
   *               | "while" "(" <exp> ")" <statement>
   *               | "do" <statement> "while" "(" <exp> ")" ";"
   *               | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
   *               | <exp> ";"
   *               | ";"
   *)
  and parse_statement tokens =
    match Tok_stream.peek tokens with
    (* "return" <exp> ";" *)
    | T.KWReturn ->
        let _ = expect T.KWReturn tokens in
        let return_val = parse_exp 0 tokens in
        let _ = expect T.Semicolon tokens in
        Ast.Return return_val
    (* ";" *)
    | T.Semicolon ->
        let _ = Tok_stream.take_token tokens in
        Ast.Null
    (* "if" "(" <exp> ")" <statement> [ "else" <statement> ] *)
    | T.KWIf ->
        let _ = Tok_stream.take_token tokens in
        let _ = expect T.OpenParen tokens in
        let condition = parse_exp 0 tokens in
        let _ = expect T.CloseParen tokens in
        let then_clause = parse_statement tokens in
        let else_clause =
          if Tok_stream.peek tokens = T.KWElse then
            let _ = Tok_stream.take_token tokens in
            Some (parse_statement tokens)
          else None
        in
        Ast.If { condition; then_clause; else_clause }
    (* <block> *)
    | T.OpenBrace -> Ast.Compound (parse_block tokens)
    (* parser only generate AST node, not given label for loops; the label will
       be given at semantic analysis phres *)
    | T.KWBreak ->
        let _ = Tok_stream.take_token tokens in
        let _ = expect T.Semicolon tokens in
        Ast.Break ""
    | T.KWContinue ->
        let _ = Tok_stream.take_token tokens in
        let _ = expect T.Semicolon tokens in
        Ast.Continue ""
    (* "while" "(" <exp> ")" <statement> *)
    | T.KWWhile ->
        let _ = Tok_stream.take_token tokens in
        let _ = expect T.OpenParen tokens in
        let condition = parse_exp 0 tokens in
        let _ = expect T.CloseParen tokens in
        let body = parse_statement tokens in
        Ast.While { condition; body; id = "" }
    (* "do" <statement> "while" "(" <exp> ")" ";" *)
    | T.KWDo ->
        let _ = Tok_stream.take_token tokens in
        let body = parse_statement tokens in
        let _ = expect T.KWWhile tokens in
        let _ = expect T.OpenParen tokens in
        let condition = parse_exp 0 tokens in
        let _ = expect T.CloseParen tokens in
        let _ = expect T.Semicolon tokens in
        Ast.DoWhile { body; condition; id = "" }
    (* "for" "(" <for_init> [ <exp> ] ";" [ <exp> ] ")" <statement> *)
    | T.KWFor ->
        let _ = Tok_stream.take_token tokens in
        let _ = expect T.OpenParen tokens in
        let init = parse_for_init tokens in
        let condition = parse_optional_exp T.Semicolon tokens in
        let post = parse_optional_exp T.CloseParen tokens in
        let body = parse_statement tokens in
        Ast.For { init; condition; post; body; id = "" }
    (* <exp> ";" *)
    | _ ->
        let exp = parse_exp 0 tokens in
        let _ = expect T.Semicolon tokens in
        Ast.Expression exp

  and parse_block_item tokens =
    if is_specifier (Tok_stream.peek tokens) then
      Ast.D (parse_declaration tokens)
    else Ast.S (parse_statement tokens)

  (* <block> ::= "{" { <block_item } "}" *)
  and parse_block tokens =
    let _ = expect T.OpenBrace tokens in
    let rec parse_block_item_loop () =
      if Tok_stream.peek tokens = T.CloseBrace then []
      else
        let next_block_item = parse_block_item tokens in
        next_block_item :: parse_block_item_loop ()
    in
    let block = parse_block_item_loop () in
    let _ = expect T.CloseBrace tokens in
    Ast.Block block

  (* <program> ::= { <declaration> } *)
  let parse_program tokens =
    let rec parse_decl_loop () =
      if Tok_stream.is_empty tokens then []
      else
        let next_decl = parse_declaration tokens in
        next_decl :: parse_decl_loop ()
    in
    let fun_decls = parse_decl_loop () in
    Ast.Program fun_decls
end

let parser tokens =
  try
    let token_stream = Tok_stream.of_list tokens in
    let ast = Private.parse_program token_stream in
    let _ = Ast_print.print_program ast in
    ast
  with Tok_stream.End_of_stream ->
    raise (ParserError "Unexpected end of file")
