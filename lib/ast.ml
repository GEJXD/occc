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

(* the exp type here are defined recursively, means that we should recursively
   parse the inner exp *)
type exp =
  | Constant of int
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
[@@deriving show]

type statement = Return of exp [@@deriving show]

type function_definition = Function of { name : string; body : statement }
[@@deriving show]

type t = Program of function_definition [@@deriving show]

module PrintAst = struct
  open Format

  let pp_unary_op fmt op =
    match op with
    | Complement -> fprintf fmt "~"
    | Negate -> fprintf fmt "-"
    | Not -> fprintf fmt "!"

  let pp_binary_op fmt op =
    match op with
    | Add -> fprintf fmt "+"
    | Subtract -> fprintf fmt "-"
    | Multiply -> fprintf fmt "*"
    | Divide -> fprintf fmt "/"
    | Mod -> fprintf fmt "%%"
    | BitAnd -> fprintf fmt "&"
    | BitOr -> fprintf fmt "|"
    | BitXor -> fprintf fmt "^"
    | ShiftLeft -> fprintf fmt "<<"
    | ShiftRight -> fprintf fmt ">>"
    | And -> fprintf fmt "&&"
    | Or -> fprintf fmt "||"
    | Equal -> fprintf fmt "=="
    | NotEqual -> fprintf fmt "!="
    | LessThan -> fprintf fmt "<"
    | LessOrEqual -> fprintf fmt "<="
    | GreaterThan -> fprintf fmt ">"
    | GreaterOrEqual -> fprintf fmt ">="

  let rec pp_exp fmt = function
    | Constant n -> fprintf fmt "%d" n
    | Unary (op, e) -> fprintf fmt "(%a%a)" pp_unary_op op pp_exp e
    | Binary (op, e1, e2) ->
        fprintf fmt "(%a %a %a)" pp_exp e1 pp_binary_op op pp_exp e2

  let pp_statement fmt (Return e) = fprintf fmt "return %a;" pp_exp e

  let pp_function_definition fmt (Function { name; body }) =
    fprintf fmt "int %s(void) {@\n  %a@\n}" name pp_statement body

  let pp_program fmt (Program fdef) = pp_function_definition fmt fdef
  let string_of_program p = asprintf "%a" pp_program p
  let print_program p = printf "%a\n%!" pp_program p
end
