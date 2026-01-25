module T = struct
  include Tacky
end

let convert_op = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate

let convert_binop = function
  | Ast.Add -> T.Add
  | Ast.Subtract -> T.Subtract
  | Ast.Multiply -> T.Multiply
  | Ast.Divide -> T.Divide
  | Ast.Mod -> T.Mod

(* return a T.instruction list * T.Var *)
let rec emit_tacky_for_exp = function
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Unary (op, inner) -> emit_unary_expression op inner
  | Ast.Binary (op, e1, e2) -> emit_binary_expression op e1 e2

and emit_unary_expression op inner =
  let instrs, src = emit_tacky_for_exp inner in
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let tacky_op = convert_op op in
  let tacky_node = T.Unary { op = tacky_op; src; dst } in
  (tacky_node :: instrs, dst)

and emit_binary_expression op e1 e2 =
  let instrs1, v1 = emit_tacky_for_exp e1 in
  let instrs2, v2 = emit_tacky_for_exp e2 in
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let tacky_op = convert_binop op in
  let tacky_node = T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst } in
  let instrs = instrs2 @ instrs1 in
  Tacky_print.TackyPrinter.print_list (List.rev instrs);
  (tacky_node :: instrs, dst)

(* return a instructions list. for now statement only have Return
   instruction. *)
let emit_tacky_for_statement (Ast.Return exp) =
  let instr_lst, dst = emit_tacky_for_exp exp in
  T.Return dst :: instr_lst |> List.rev

let emit_tacky_for_function (Ast.Function { name; body }) =
  let instr_lst = emit_tacky_for_statement body in
  T.Function { name; body = instr_lst }

let tacky_gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
