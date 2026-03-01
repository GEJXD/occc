type unary_operator = Complement | Negate | Not [@@deriving show]

type binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | BitAnd
  | BitOr
  | BitXor
  | ShiftLeft
  | ShiftRight
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual
[@@deriving show]

type compound_operator =
  | AddAssign (* += *)
  | SubtractAssign (* -= *)
  | MultiplyAssign (* *= *)
  | DivideAssign (* /= *)
  | ModAssign (* %= *)
  | BitAndAssign (* &= *)
  | BitOrAssign (* |= *)
  | BitXorAssign (* ^= *)
  | ShiftLeftAssign (* <<= *)
  | ShiftRightAssign (* >>= *)
[@@deriving show]

(* the exp type here are defined recursively, means that we should recursively
   parse the inner exp *)
type exp =
  | Constant of int
  | Var of string
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
  | Assignment of exp * exp
  | CompoundAssign of compound_operator * exp * exp
  | Conditional of { condition : exp; then_result : exp; else_result : exp }
  | FunCall of { f : string; args : exp list }
[@@deriving show]

type storage_class = Static | Extern [@@deriving show]

type variable_declaration = {
  name : string;
  init : exp option;
  storage_class : storage_class option;
}
[@@deriving show]

type for_init = InitDecl of variable_declaration | InitExp of exp option
[@@deriving show]

type statement =
  | Return of exp
  | Expression of exp
  | If of {
      condition : exp;
      then_clause : statement;
      else_clause : statement option;
    }
  | Compound of block
  | Break of string
  | Continue of string
  | While of { condition : exp; body : statement; id : string }
  | DoWhile of { body : statement; condition : exp; id : string }
  | For of {
      init : for_init;
      condition : exp option;
      post : exp option;
      body : statement;
      id : string;
    }
  | Null (* a single semicolon, means do nothing *)
[@@deriving show]

and block_item = S of statement | D of declaration [@@deriving show]
and block = Block of block_item list [@@deriving show]

and function_declaration = {
  name : string;
  params : string list;
  body : block option;
  storage_class : storage_class option;
}
[@@deriving show]

and declaration =
  | FunDecl of function_declaration
  | VarDecl of variable_declaration
[@@deriving show]

type t = Program of declaration list [@@deriving show]
