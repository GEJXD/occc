module T = struct
  include Tacky
end

let break_label id = "break." ^ id
let continue_label id = "continue." ^ id

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
  | Ast.FunCall { f; args } -> emit_fun_call f args

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

and emit_fun_call f args =
  let dst_name = Unique_ids.make_temporary () in
  let dst = T.Var dst_name in
  let arg_instructions, arg_vals =
    List.split (List.map emit_tacky_for_exp args)
  in
  let instructions =
    List.flatten arg_instructions @ [ T.FunCall { f; args = arg_vals; dst } ]
  in
  (instructions, dst)

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
  (* breaks and continues in same loop are have same label so break_label will
     have same label in both Ast.Break and Ast.DoWhile *)
  | Ast.Break id -> [ T.Jump (break_label id) ]
  | Ast.Continue id -> [ T.Jump (continue_label id) ]
  | Ast.DoWhile { body; condition; id } ->
      emit_tacky_for_do_loop body condition id
  | Ast.While { condition; body; id } ->
      emit_tacky_for_while_loop condition body id
  | Ast.For { init; condition; post; body; id } ->
      emit_tacky_for_for_loop init condition post body id
  | Ast.Null -> []

and emit_tacky_for_block_item = function
  | Ast.S s -> emit_tacky_for_statement s
  | Ast.D d -> emit_local_declaration d

and emit_local_declaration = function
  | Ast.VarDecl { storage_class = Some _; _ } -> []
  | Ast.VarDecl s -> emit_var_declaration s
  (* there are only can have function declarations in block. *)
  | Ast.FunDecl _ -> []

and emit_var_declaration = function
  (* a variable declaration with a initializer *)
  | Ast.{ name; init = Some e; _ } ->
      let eval_assignment, _assign_result =
        emit_tacky_for_exp (Ast.Assignment (Var name, e))
      in
      eval_assignment
  (* do not need to have any instructions for declaration without initializer *)
  | Ast.{ init = None; _ } -> []

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

and emit_tacky_for_do_loop body condition id =
  let start_label = Unique_ids.make_label "do_loop_start" in
  let cont_label = continue_label id in
  let br_label = break_label id in
  let eval_condition, c = emit_tacky_for_exp condition in
  (T.Label start_label :: emit_tacky_for_statement body)
  @ (T.Label cont_label :: eval_condition)
  @ [ T.JumpIfNotZero (c, start_label); T.Label br_label ]

and emit_tacky_for_while_loop condition body id =
  let cont_label = continue_label id in
  let br_label = break_label id in
  let eval_condition, c = emit_tacky_for_exp condition in
  (T.Label cont_label :: eval_condition)
  @ (T.JumpIfZero (c, br_label) :: emit_tacky_for_statement body)
  @ [ T.Jump cont_label; T.Label br_label ]

and emit_tacky_for_for_loop init condition post body id =
  let start_label = Unique_ids.make_label "for_start" in
  let cont_label = continue_label id in
  let br_label = break_label id in
  let for_init_instructions =
    match init with
    | Ast.InitDecl d -> emit_var_declaration d
    | Ast.InitExp e -> begin
        match Option.map emit_tacky_for_exp e with
        | Some (instrs, _) -> instrs
        | None -> []
      end
  in
  let test_condition =
    match Option.map emit_tacky_for_exp condition with
    | Some (instrs, v) -> instrs @ [ T.JumpIfZero (v, br_label) ]
    | None -> []
  in
  let post_instructions =
    match Option.map emit_tacky_for_exp post with
    | Some (instrs, _) -> instrs
    | None -> []
  in
  for_init_instructions
  @ (T.Label start_label :: test_condition)
  @ emit_tacky_for_statement body
  @ (T.Label cont_label :: post_instructions)
  @ [ T.Jump start_label; T.Label br_label ]

let emit_fun_declaration = function
  | Ast.FunDecl { name; params; body = Some (Block block_items); _ } ->
      let global = Symbols.is_global name in
      let body_instructions =
        List.concat_map emit_tacky_for_block_item block_items
      in
      let extra_return = T.(Return (Constant 0)) in
      Some
        (T.Function
           { name; global; params; body = body_instructions @ [ extra_return ] })
  | _ -> None

let convert_symbols_to_tacky all_symbols =
  let to_var (name, entry) =
    match entry.Symbols.attrs with
    | Symbols.StaticAttr { init; global } -> (
        match init with
        | Initial i -> Some (T.StaticVariable { name; global; init = i })
        | Tentative -> Some (StaticVariable { name; global; init = 0 })
        | NoInitializer -> None)
    | _ -> None
  in
  List.filter_map to_var all_symbols

let tacky_gen (Ast.Program decls) =
  let tacky_fn_defs = List.filter_map emit_fun_declaration decls in
  let tacky_var_defs = convert_symbols_to_tacky (Symbols.bindings ()) in
  Tacky.Program (tacky_var_defs @ tacky_fn_defs)
