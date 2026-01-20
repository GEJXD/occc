let convert_exp (Ast.Constant i) = Assembly.Imm i

let convert_statement (Ast.Return e) =
  let value = convert_exp e in
  Assembly.[Mov (value, Register); Ret]

let convert_function (Ast.Function {name; body}) =
  let inst_list = convert_statement body in
  Assembly.Function {name; instructions= inst_list}

let codegen (Ast.Program func_def) =
  Assembly.Program (convert_function func_def)
