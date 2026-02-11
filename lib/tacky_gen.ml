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

let convert_comop = function
  | Ast.AddAssign -> T.Add
  | Ast.SubtractAssign -> T.Subtract
  | Ast.MultiplyAssign -> T.Multiply
  | Ast.DivideAssign -> T.Divide
  | Ast.ModAssign -> T.Mod
  | Ast.BitAndAssign -> T.BitAnd
  | Ast.BitOrAssign -> T.BitOr
  | Ast.BitXorAssign -> T.BitXor
  | Ast.ShiftLeftAssign -> T.ShiftLeft
  | Ast.ShiftRightAssign -> T.ShiftRight

(* return a T.instruction list * T.Var, means (instr_lst, result) *)
let rec emit_tacky_for_exp = function
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Var v -> ([], T.Var v)
  | Ast.Unary (op, inner) -> emit_unary_expression op inner
  | Ast.Binary (Ast.And, e1, e2) -> emit_and_expression e1 e2
  | Ast.Binary (Ast.Or, e1, e2) -> emit_or_expressiont e1 e2
  | Ast.Binary (op, e1, e2) -> emit_binary_expression op e1 e2
  | Ast.Assignment (Ast.Var v, rhs) ->
      let rhs_instructions, rhs_result = emit_tacky_for_exp rhs in
      let instr_lst =
        rhs_instructions @ [ T.Copy { src = rhs_result; dst = T.Var v } ]
      in
      (instr_lst, rhs_result)
  | Ast.Assignment _ -> failwith "Internal Error: bad lvalue"
  | Ast.CompoundAssign (op, Ast.Var v, rhs) ->
      emit_compound_assignment_expression op v rhs
  | Ast.CompoundAssign _ -> failwith "Internal Error: bad lvalue"
  | Ast.Conditional { condition; then_result; else_result } ->
      emit_conditional_expression condition then_result else_result

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

and emit_compound_assignment_expression op v rhs =
  let rhs_instructions, rhs_result = emit_tacky_for_exp rhs in
  let var_value = T.Var v in
  let temp_result = T.Var (Unique_ids.make_temporary ()) in
  let binary_op = convert_comop op in
  let binary_instr =
    T.Binary
      { op = binary_op; src1 = var_value; src2 = rhs_result; dst = temp_result }
  in
  let copy_instr = T.Copy { src = temp_result; dst = var_value } in
  let instr_lst = rhs_instructions @ [ binary_instr; copy_instr ] in
  (instr_lst, temp_result)

and emit_conditional_expression condition then_result else_result =
  let eval_cond, c = emit_tacky_for_exp condition in
  let eval_v1, v1 = emit_tacky_for_exp then_result in
  let eval_v2, v2 = emit_tacky_for_exp else_result in
  let e2_label = Unique_ids.make_label "conditional_else" in
  let end_label = Unique_ids.make_label "conditional_end" in
  let dst = T.Var (Unique_ids.make_temporary ()) in
  let instr_lst =
    eval_cond
    @ (T.JumpIfZero (c, e2_label) :: eval_v1)
    @ T.Copy { src = v1; dst }
      :: T.Jump end_label :: T.Label e2_label :: eval_v2
    @ T.[ Copy { src = v2; dst }; Label end_label ]
  in
  (instr_lst, dst)

(* return a instructions list. for now statement only have Return
   instruction. *)
let rec emit_tacky_for_statement = function
  | Ast.Return exp ->
      let eval_exp, result = emit_tacky_for_exp exp in
      eval_exp @ [ T.Return result ]
  | Ast.Expression exp ->
      let eval_exp, _ = emit_tacky_for_exp exp in
      eval_exp
  | Ast.If { condition; then_clause; else_clause } ->
      emit_tacky_for_if_statement condition then_clause else_clause
  | Ast.Compound (Block items) ->
      List.concat_map emit_tacky_for_block_item items
  | Ast.Null -> []

and emit_tacky_for_if_statement condition then_clause = function
  | None ->
      let eval_cond, c = emit_tacky_for_exp condition in
      let end_label = Unique_ids.make_label "if_end" in
      eval_cond
      @ (T.JumpIfZero (c, end_label) :: emit_tacky_for_statement then_clause)
      @ [ T.Label end_label ]
  | Some else_clause ->
      let eval_cond, c = emit_tacky_for_exp condition in
      let end_label = Unique_ids.make_label "if_end" in
      let else_label = Unique_ids.make_label "else" in
      eval_cond
      @ (T.JumpIfZero (c, else_label) :: emit_tacky_for_statement then_clause)
      @ T.Jump end_label :: T.Label else_label
        :: emit_tacky_for_statement else_clause
      @ [ T.Label end_label ]

and emit_tacky_for_block_item = function
  | Ast.S s -> emit_tacky_for_statement s
  | Ast.D (Declaration { name; init = Some e }) ->
      let eval_assignment, _ =
        emit_tacky_for_exp (Ast.Assignment (Var name, e))
      in
      eval_assignment
  | Ast.D (Declaration { init = None; _ }) -> []

let emit_tacky_for_function (Ast.Function { name; body = Block block_items }) =
  let body_instructions =
    List.concat_map emit_tacky_for_block_item block_items
  in
  let extra_return = T.(Return (Constant 0)) in
  T.Function { name; body = body_instructions @ [ extra_return ] }

let tacky_gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
