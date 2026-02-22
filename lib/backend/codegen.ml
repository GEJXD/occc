let param_passing_regs = Assembly.[ DI; SI; DX; CX; R8; R9 ]

let convert_val = function
  | Tacky.Constant c -> Assembly.Imm c
  | Tacky.Var id -> Assembly.Pseudo id

let convert_unop = function
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg
  | Tacky.Not ->
      failwith "Internal error: can't convert TACKY not directly to assembly."

let convert_binop = function
  | Tacky.Add -> Assembly.Add
  | Tacky.Subtract -> Assembly.Sub
  | Tacky.Multiply -> Assembly.Mult
  | Tacky.BitXor -> Assembly.Xor
  | Tacky.BitOr -> Assembly.Or
  | Tacky.BitAnd -> Assembly.And
  | Tacky.ShiftLeft -> Assembly.Shl
  | Tacky.ShiftRight -> Assembly.Sar
  | Tacky.(
      ( Divide | Mod | Equal | NotEqual | GreaterOrEqual | LessOrEqual
      | GreaterThan | LessThan )) ->
      failwith "Internal error: not a binary assembly instruction"

let convert_cond_code = function
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.LessThan -> Assembly.L
  | Tacky.LessOrEqual -> Assembly.LE
  | Tacky.GreaterOrEqual -> Assembly.GE
  | Tacky.GreaterThan -> Assembly.G
  | _ -> failwith "Interal error: not a condition op"

let convert_function_call f args dst =
  let reg_args, stack_args = Utils.ListUtil.take_drop 6 args in
  let stack_padding = if List.length stack_args mod 2 = 0 then 0 else 8 in
  let padding_stack_instructions =
    if stack_padding = 0 then [] else [ Assembly.AllocateStack stack_padding ]
  in
  (* pass args in registers *)
  let pass_reg_args idx arg =
    let reg = List.nth param_passing_regs idx in
    let assembly_arg = convert_val arg in
    Assembly.Mov (assembly_arg, Reg reg)
  in
  let prepare_args_to_regs =
    padding_stack_instructions @ List.mapi pass_reg_args reg_args
  in
  (* pass args on stack *)
  let pass_stack_arg arg =
    let assembly_arg = convert_val arg in
    match assembly_arg with
    | Assembly.Imm _ | Reg _ -> [ Assembly.Push assembly_arg ]
    | _ -> Assembly.[ Mov (assembly_arg, Reg AX); Push (Reg AX) ]
  in
  let prepare_args_to_stack =
    prepare_args_to_regs @ List.concat (List.rev_map pass_stack_arg stack_args)
  in
  let function_call = prepare_args_to_stack @ [ Assembly.Call f ] in
  let bytes_to_remove = (8 * List.length stack_args) + stack_padding in
  let dealloc_stack =
    if bytes_to_remove = 0 then []
    else [ Assembly.DeallocateStack bytes_to_remove ]
  in
  let instructions = function_call @ dealloc_stack in
  (* mov return value to dst *)
  let assembly_dst = convert_val dst in
  instructions @ [ Mov (Reg AX, assembly_dst) ]

let convert_instruction = function
  | Tacky.Copy { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      Assembly.[ Mov (asm_src, asm_dst) ]
  | Tacky.Return tacky_val ->
      let asm_val = convert_val tacky_val in
      Assembly.[ Mov (asm_val, Reg AX); Ret ]
  | Tacky.Unary { op = Not; src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      Assembly.
        [ Cmp (Imm 0, asm_src); Mov (Imm 0, asm_dst); SetCC (E, asm_dst) ]
  | Tacky.Unary { op; src; dst } ->
      let asm_op = convert_unop op in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      Assembly.[ Mov (asm_src, asm_dst); Unary (asm_op, asm_dst) ]
  | Tacky.Binary { op; src1; src2; dst } -> (
      let asm_src1 = convert_val src1 in
      let asm_src2 = convert_val src2 in
      let asm_dst = convert_val dst in
      match op with
      | Tacky.Equal | Tacky.NotEqual | Tacky.LessThan | Tacky.LessOrEqual
      | Tacky.GreaterThan | Tacky.GreaterOrEqual ->
          let cond_code = convert_cond_code op in
          Assembly.
            [
              Cmp (asm_src2, asm_src1);
              Mov (Imm 0, asm_dst);
              SetCC (cond_code, asm_dst);
            ]
      | Tacky.Divide | Tacky.Mod ->
          let result_reg =
            if op = Tacky.Divide then Assembly.AX else Assembly.DX
          in
          Assembly.
            [
              Mov (asm_src1, Reg AX);
              Cdq;
              Idiv asm_src2;
              Mov (Reg result_reg, asm_dst);
            ]
      (* shl and shr only permit %cl as the src operand *)
      | Tacky.ShiftLeft | Tacky.ShiftRight ->
          let shift_op =
            if op = Tacky.ShiftLeft then Assembly.Shl else Assembly.Sar
          in
          let prep_count, count_operand =
            match asm_src2 with
            | Assembly.Imm _ as imm -> ([], imm)
            | _ ->
                ( [ Assembly.Mov (asm_src2, Reg Assembly.CX) ],
                  Assembly.Reg Assembly.CX )
          in
          Assembly.(
            (Mov (asm_src1, asm_dst) :: prep_count)
            @ [ Binary { op = shift_op; src = count_operand; dst = asm_dst } ])
      | _ ->
          let asm_op = convert_binop op in
          Assembly.
            [
              Mov (asm_src1, asm_dst);
              Binary { op = asm_op; src = asm_src2; dst = asm_dst };
            ])
  | Tacky.Jump target -> Assembly.[ Jmp target ]
  | Tacky.JumpIfZero (cond, target) ->
      let asm_cond_val = convert_val cond in
      Assembly.[ Cmp (Imm 0, asm_cond_val); JmpCC (E, target) ]
  | Tacky.JumpIfNotZero (cond, target) ->
      let asm_cond_val = convert_val cond in
      Assembly.[ Cmp (Imm 0, asm_cond_val); JmpCC (NE, target) ]
  | Tacky.Label label -> Assembly.[ Label label ]
  | Tacky.FunCall { f; args; dst } -> convert_function_call f args dst

(* copy params to stack *)
let pass_params params =
  let reg_params, stack_params = Utils.ListUtil.take_drop 6 params in
  let pass_in_register idx param =
    let reg = List.nth param_passing_regs idx in
    Assembly.Mov (Reg reg, Pseudo param)
  in
  let pass_on_stack idx param =
    let stk = Assembly.Stack (16 + (8 * idx)) in
    Assembly.Mov (stk, Pseudo param)
  in
  List.mapi pass_in_register reg_params @ List.mapi pass_on_stack stack_params

let convert_function (Tacky.Function { name; params; body }) =
  let instructions =
    pass_params params @ List.concat_map convert_instruction body
  in
  Assembly.Function { name; instructions }

let codegen (Tacky.Program fn_defs) =
  let assembly_dn_defs = List.map convert_function fn_defs in
  Assembly.Program assembly_dn_defs
