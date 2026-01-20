module T = struct
  include Tacky
end

let convert_op = function
  | Ast.Complement -> T.Complement
  | Ast.Negate -> T.Negate

(* return a T.instruction list * T.Var *)
let rec emit_tacky_for_exp = function
  | Ast.Constant c -> ([], T.Constant c)
  | Ast.Unary (op, inner) ->
      let instrs, src = emit_tacky_for_exp inner in
      let dst = T.Var (Unique_ids.make_temporary ()) in
      let tacky_op = convert_op op in
      let tacky_node = T.Unary { op = tacky_op; src; dst } in
      (tacky_node :: instrs, dst)

(* return a instructions list *)
let emit_tacky_for_statement (Ast.Return exp) =
  let instr_lst, value = emit_tacky_for_exp exp in
  T.Return value :: instr_lst |> List.rev

let emit_tacky_for_function (Ast.Function { name; body }) =
  let instr_lst = emit_tacky_for_statement body in
  T.Function { name; body = instr_lst }

let tacky_gen (Ast.Program fn_def) = T.Program (emit_tacky_for_function fn_def)
