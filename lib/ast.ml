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
[@@deriving show]

type declaration = Declaration of { name : string; init : exp option }
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
  | Null (* a single semicolon, means do nothing *)
[@@deriving show]

and block_item = S of statement | D of declaration [@@deriving show]
and block = Block of block_item list

type function_definition = Function of { name : string; body : block }
[@@deriving show]

type t = Program of function_definition [@@deriving show]
