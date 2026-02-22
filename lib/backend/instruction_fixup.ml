open Assembly

let fixup_instruction = function
  (* MOV instructions can not have two memory operand in same time. *)
  | Mov ((Stack _ as src), (Stack _ as dst)) ->
      [ Mov (src, Reg R10); Mov (Reg R10, dst) ]
  | Idiv (Imm i) -> [ Mov (Imm i, Reg R10); Idiv (Reg R10) ]
  | Binary
      {
        op = (Add | Sub | And | Or | Xor | Shl | Sar) as op;
        src = Stack _ as src;
        dst = Stack _ as dst;
      } ->
      [ Mov (src, Reg R10); Binary { op; src = Reg R10; dst } ]
  | Binary { op = Mult; src; dst = Stack _ as dst } ->
      [
        Mov (dst, Reg R11);
        Binary { op = Mult; src; dst = Reg R11 };
        Mov (Reg R11, dst);
      ]
  | Cmp ((Stack _ as src), (Stack _ as dst)) ->
      [ Mov (src, Reg R10); Cmp (Reg R10, dst) ]
  | Cmp (src, Imm i) -> [ Mov (Imm i, Reg R11); Cmp (src, Reg R11) ]
  | other -> [ other ]

let fixup_function last_stack_slot (Function { name; instructions }) =
  Function
    {
      name;
      instructions =
        AllocateStack (-last_stack_slot)
        :: List.concat_map fixup_instruction instructions;
    }

let fixup_function (Function { name; instructions }) =
  let stack_bytes = -(Symbols.get name).stack_frame_size in
  Function
    {
      name;
      instructions =
        AllocateStack (Rounding.round_away_from_zero 16 stack_bytes)
        :: List.concat_map fixup_instruction instructions;
    }

let fixup_program (Program fn_defs) =
  let fixed_fn_defs = List.map fixup_function fn_defs in
  Program fixed_fn_defs
