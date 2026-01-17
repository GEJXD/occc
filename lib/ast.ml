type exp = Constant of int
type statement = Return of exp
type function_definition = Function of 
  {name : string; body : statement}

type t = Program of function_definition

module PrintAst = struct
  open Format

  let pp_exp fmt (Constant n) =
    fprintf fmt "%d" n

  let pp_statement fmt (Return e) =
    fprintf fmt "return %a;" pp_exp e

  let pp_function_definition fmt (Function {name; body}) =
    fprintf fmt "int %s(void) {@\n  %a@\n}" name pp_statement body

  let pp_program fmt (Program fdef) =
    pp_function_definition fmt fdef

  let string_of_program p = asprintf "%a" pp_program p

  let print_program p = printf "%a\n%!" pp_program p
end
