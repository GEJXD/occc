type unary_operator = Complement | Negate

(* the exp type here are defined recursively, means that we should recursively
   parse the inner exp *)
type exp = Constant of int | Unary of unary_operator * exp
type statement = Return of exp
type function_definition = Function of { name : string; body : statement }
type t = Program of function_definition

module PrintAst = struct
  open Format

  let pp_unary_op fmt op =
    match op with Complement -> fprintf fmt "~" | Negate -> fprintf fmt "-"

  let rec pp_exp fmt = function
    | Constant n -> fprintf fmt "%d" n
    | Unary (op, e) -> fprintf fmt "%a%a" pp_unary_op op pp_exp e

  let pp_statement fmt (Return e) = fprintf fmt "return %a;" pp_exp e

  let pp_function_definition fmt (Function { name; body }) =
    fprintf fmt "int %s(void) {@\n  %a@\n}" name pp_statement body

  let pp_program fmt (Program fdef) = pp_function_definition fmt fdef
  let string_of_program p = asprintf "%a" pp_program p
  let print_program p = printf "%a\n%!" pp_program p
end
