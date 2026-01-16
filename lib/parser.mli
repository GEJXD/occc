exception ParserError of string

val parser : Tokens.t list -> Ast.t
