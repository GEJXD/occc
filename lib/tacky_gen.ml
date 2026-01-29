module T = struct
  include Tacky
end

let convert_op = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate
  | Ast.Not -> T.Not

let convert_binop = function
  | Ast.Add -> T.Add
  | Ast.Subtract -> T.Subtract
  | Ast.Multiply -> T.Multiply
  | Ast.Divide -> T.Divide
  | Ast.Mod -> T.Mod
  | Ast.BitAnd -> T.BitAnd
  | Ast.BitOr -> T.BitOr
  | Ast.BitXor -> T.BitXor
  | Ast.ShiftLeft -> T.ShiftLeft
  | Ast.ShiftRight -> T.ShiftRight
  | Ast.Equal -> T.Equal
  | Ast.NotEqual -> T.NotEqual
  | Ast.LessThan -> T.LessThan
  | Ast.LessOrEqual -> T.LessOrEqual
  | Ast.GreaterThan -> T.GreaterThan
  | Ast.GreaterOrEqual -> T.GreaterOrEqual
  | Ast.And | Ast.Or ->
      failwith "Internal error. cannot conovert these directly to TACKY binops"

(* return a T.instruction list * T.Var *)
let rec emit_tacky_for_exp = function
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Unary (op, inner) -> emit_unary_expression op inner
  | Ast.Binary (Ast.And, e1, e2) -> emit_and_expression e1 e2
  | Ast.Binary (Ast.Or, e1, e2) -> emit_or_expressiont e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_expression op e1 e2

and emit_unary_expression op inner =
  let eval_inner, v = emit_tacky_for_exp inner in
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let tacky_op = convert_op op in
  let tacky_node = T.Unary { op = tacky_op; src = v; dst } in
  let instr_lst = eval_inner @ [ tacky_node ] in
  (instr_lst, dst)

(* give up to write tail recursive T_T *)
and emit_binary_expression op e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let tacky_op = convert_binop op in
  let tacky_node = T.Binary { op = tacky_op; src1 = v1; src2 = v2; dst } in
  let instr_lst = eval_v1 @ eval_v2 @ [ tacky_node ] in
  (instr_lst, dst)

and emit_and_expression e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let false_label = Unique_ids.make_label "and_false" in
  let end_label = Unique_ids.make_label "and_end" in
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let instr_lst =
    eval_v1
    @ [ T.JumpIfZero (v1, false_label) ]
    @ eval_v2
    @ [
        T.JumpIfZero (v2, false_label);
        T.Copy { src = T.Constant 1; dst };
        T.Jump end_label;
        T.Label false_label;
        T.Copy { src = T.Constant 0; dst };
        T.Label end_label;
      ]
  in
  (instr_lst, dst)

and emit_or_expressiont e1 e2 =
  let eval_v1, v1 = emit_tacky_for_exp e1 in
  let eval_v2, v2 = emit_tacky_for_exp e2 in
  let false_label = Unique_ids.make_label "or_false" in
  let end_label = Unique_ids.make_label "or_end" in
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let instr_lst =
    eval_v1
    @ [ T.JumpIfNotZero (v1, false_label) ]
    @ eval_v2
    @ [
        T.JumpIfNotZero (v2, false_label);
        T.Copy { src = T.Constant 0; dst };
        T.Jump end_label;
        T.Label false_label;
        T.Copy { src = T.Constant 1; dst };
        T.Label end_label;
      ]
  in
  (instr_lst, dst)

(* return a instructions list. for now statement only have Return
   instruction. *)
let emit_tacky_for_statement (Ast.Return exp) =
  let instr_lst, dst = emit_tacky_for_exp exp in
  instr_lst @ [ T.Return dst ]

let emit_tacky_for_function (Ast.Function { name; body }) =
  let instr_lst = emit_tacky_for_statement body in
  Tacky_print.TackyPrinter.print_list instr_lst;
  T.Function { name; body = instr_lst }

let tacky_gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
