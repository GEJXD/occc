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

let convert_function (Tacky.Function { name; body }) =
  let instructions = List.concat_map convert_instruction body in
  Assembly.Function { name; instructions }

let codegen (Tacky.Program func_def) =
  Assembly.Program (convert_function func_def)
