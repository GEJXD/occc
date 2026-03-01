open Assembly

let align_directive =
  match !Settings.platform with OS_X -> ".balign" | Linux -> ".align"

let show_reg = function
  | AX -> "%eax"
  | CX -> "%ecx"
  | DX -> "%edx"
  | DI -> "%edi"
  | SI -> "%esi"
  | R8 -> "%r8d"
  | R9 -> "%r9d"
  | R10 -> "%r10d"
  | R11 -> "%r11d"

let show_label label =
  match !Settings.platform with OS_X -> "_" ^ label | Linux -> label

let show_local_label label =
  match !Settings.platform with OS_X -> "L" ^ label | Linux -> ".L" ^ label

let show_fn_name f =
  match !Settings.platform with
  | OS_X -> "_" ^ f
  | Linux -> if Symbols.is_defined f then f else f ^ "@PLT"

let show_operand = function
  | Reg reg -> show_reg reg
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Imm i -> Printf.sprintf "$%d" i
  (* print pseudoregister is only for debugging *)
  | Pseudo name -> Printf.sprintf "%%%s" name
  | Data name -> Printf.sprintf "%s(%%rip)" (show_label name) [@coverage off]

let show_byte_reg = function
  | AX -> "%al"
  | CX -> "%cl"
  | DX -> "%dl"
  | DI -> "%dil"
  | SI -> "%sil"
  | R8 -> "%r8b"
  | R9 -> "%r9b"
  | R10 -> "%r10b"
  | R11 -> "r%11b"

let show_byte_operand = function
  | Reg reg -> show_byte_reg reg
  | other -> show_operand other

let show_quadword_reg = function
  | AX -> "%rax"
  | CX -> "%rcx"
  | DX -> "%rdx"
  | DI -> "%rdi"
  | SI -> "%rsi"
  | R8 -> "%r8"
  | R9 -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"

let show_quadword_operand = function
  | Reg reg -> show_quadword_reg reg
  | other -> show_operand other

let show_unary_instruction = function Neg -> "negl" | Not -> "notl"

let show_binary_instruction = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"
  | And -> "andl"
  | Or -> "orl"
  | Xor -> "xorl"
  | Shl -> "shll"
  | Sar -> "sarl"

let show_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

let emit_instruction chan = function
  | Mov (src, dst) ->
      Printf.fprintf chan "\tmovl %s, %s\n" (show_operand src)
        (show_operand dst)
  | Unary (op, dst) ->
      Printf.fprintf chan "\t%s %s\n"
        (show_unary_instruction op)
        (show_operand dst)
  | Binary { op; src; dst } ->
      let src_repr =
        match (op, src) with
        | (Shl | Sar), Reg CX -> "\t%cl"
        | _ -> show_operand src
      in
      Printf.fprintf chan "\t%s %s, %s\n"
        (show_binary_instruction op)
        src_repr (show_operand dst)
  | Cmp (src, dst) ->
      Printf.fprintf chan "\tcmpl %s, %s\n" (show_operand src)
        (show_operand dst)
  | Idiv operand -> Printf.fprintf chan "\tidivl %s\n" (show_operand operand)
  | Cdq -> Printf.fprintf chan "\tcdq\n"
  | Jmp label -> Printf.fprintf chan "\tjmp %s\n" (show_local_label label)
  | JmpCC (code, label) ->
      Printf.fprintf chan "\tj%s %s\n" (show_cond_code code)
        (show_local_label label)
  | SetCC (code, operand) ->
      Printf.fprintf chan "\tset%s %s\n" (show_cond_code code)
        (show_byte_operand operand)
  | Label label -> Printf.fprintf chan "%s:\n" (show_local_label label)
  | AllocateStack i -> Printf.fprintf chan "\tsubq $%d, %%rsp\n\n" i
  | DeallocateStack i -> Printf.fprintf chan "\taddq $%d, %%rsp\n\n" i
  | Push op -> Printf.fprintf chan "\tpushq %s\n" (show_quadword_operand op)
  | Call f -> Printf.fprintf chan "\tcall %s\n" (show_fn_name f)
  | Ret ->
      Printf.fprintf chan {|
    movq %%rbp, %%rsp
    popq %%rbp
    ret
|}

let emit_global_directive chan global label =
  if global then Printf.fprintf chan "\t.globl %s\n" label else ()

let emit_top_level chan = function
  | Function { name; global; instructions } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan
        {|
    .text
%s:
    pushq %%rbp
    movq %%rsp, %%rbp
  |}
        label;
      List.iter (emit_instruction chan) instructions
  | StaticVariable { name; global; init = 0 } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan
        {|
    .bss
    %s 4
%s:
    .zero 4
|}
        align_directive label
  | StaticVariable { name; global; init } ->
      let label = show_label name in
      emit_global_directive chan global label;
      Printf.fprintf chan {|
  .data
  %s 4
%s:
  .long %d
|} align_directive
        label init

let emit_stack_note chan =
  match !Settings.platform with
  | OS_X -> ()
  | Linux -> Printf.fprintf chan "\t.section .note.GNU-stack,\"\",@progbits\n"

(* show program *)
let emit assembly_file (Program top_levels) =
  let output_channel = open_out assembly_file in
  List.iter (emit_top_level output_channel) top_levels;
  emit_stack_note output_channel;
  close_out output_channel
