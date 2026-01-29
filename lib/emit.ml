open Assembly

let show_operand = function
  | Reg AX -> "%eax"
  | Reg DX -> "%edx"
  | Reg CX -> "%ecx"
  | Reg R10 -> "%r10d"
  | Reg R11 -> "%r11d"
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Imm i -> Printf.sprintf "$%d" i
  (* print pseudoregister is only for debugging *)
  | Pseudo name -> Printf.sprintf "%%%s" name

let show_byte_operand = function
  | Reg AX -> "%al"
  | Reg DX -> "%dl"
  | Reg CX -> "%cl"
  | Reg R10 -> "%r10b"
  | Reg R11 -> "%r11b"
  | other -> show_operand other

let show_label label =
  match !Settings.platform with OS_X -> "_" ^ label | Linux -> label

let show_local_label label =
  match !Settings.platform with OS_X -> "L" ^ label | Linux -> ".L" ^ label

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
      Printf.fprintf chan "    movl %s, %s\n" (show_operand src)
        (show_operand dst)
  | Unary (op, dst) ->
      Printf.fprintf chan "    %s %s\n"
        (show_unary_instruction op)
        (show_operand dst)
  | Binary { op; src; dst } ->
      let src_repr =
        match (op, src) with
        | (Shl | Sar), Reg CX -> "    %cl"
        | _ -> show_operand src
      in
      Printf.fprintf chan "    %s %s, %s\n"
        (show_binary_instruction op)
        src_repr (show_operand dst)
  | Cmp (src, dst) ->
      Printf.fprintf chan "    cmpl %s, %s\n" (show_operand src)
        (show_operand dst)
  | Idiv operand -> Printf.fprintf chan "    idivl %s\n" (show_operand operand)
  | Cdq -> Printf.fprintf chan "cdq\n"
  | Jmp label -> Printf.fprintf chan "    jmp %s\n" (show_local_label label)
  | JmpCC (code, label) ->
      Printf.fprintf chan "    j%s %s\n" (show_cond_code code)
        (show_local_label label)
  | SetCC (code, operand) ->
      Printf.fprintf chan "    set%s %s\n" (show_cond_code code)
        (show_byte_operand operand)
  | Label label -> Printf.fprintf chan "%s:\n" (show_local_label label)
  | AllocateStack i -> Printf.fprintf chan "    subq $%d, %%rsp\n\n" i
  | Ret ->
      Printf.fprintf chan {|
    movq %%rbp, %%rsp
    popq %%rbp
    ret
|}

let emit_function chan (Function { name; instructions }) =
  let label = show_label name in
  Printf.fprintf chan
    {|
  .globl %s
%s:
    pushq %%rbp
    movq %%rsp, %%rbp
|}
    label label;
  List.iter (emit_instruction chan) instructions

let emit_stack_note chan =
  match !Settings.platform with
  | OS_X -> ()
  | Linux -> Printf.fprintf chan "    .section .note.GNU-stack,\"\",@progbits\n"

(* show program *)
let emit assembly_file (Program function_def) =
  let output_channel = open_out assembly_file in
  emit_function output_channel function_def;
  emit_stack_note output_channel;
  close_out output_channel
