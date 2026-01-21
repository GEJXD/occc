open Assembly

let show_operand = function
  | Reg AX -> "%eax"
  | Reg R10 -> "%r10d"
  | Stack i -> Printf.sprintf "%d(%%rbp)" i
  | Imm i -> Printf.sprintf "$%d" i
  (* print pseudoregister is only for debugging *)
  | Pseudo name -> Printf.sprintf "%%%s" name

let show_label name =
  match !Settings.platform with OS_X -> "_" ^ name | Linux -> name

let show_unary_instruction = function
  | Neg -> "negl"
  | Not -> "notl"

let emit_instruction chan = function
  | Mov (src, dst) ->
      Printf.fprintf chan "movl %s, %s\n"
      (show_operand src)
      (show_operand dst)
  | Unary (op, dst) ->
      Printf.fprintf chan "  %s %s\n"
      (show_unary_instruction op)
      (show_operand dst)
  | AllocateStack i -> 
    Printf.fprintf chan "  subq $%d, %%rsp\n" i
  | Ret ->
      Printf.fprintf chan {|
  movq %%rbp, %%rsp
  popq %%rbp
  ret
|}

let emit_function chan (Function { name; instructions }) =
  let label = show_label name in
  Printf.fprintf chan {|
  .globl %s
%s:
  pushq %%rbp
  movq %%rsp, %%rbp
|} label label;
  List.iter (emit_instruction chan) instructions

let emit_stack_note chan =
  match !Settings.platform with
  | OS_X -> ()
  | Linux -> Printf.fprintf chan "  .section .note.GNU-stack,\"\",@progbits\n"

(* show program *)
let emit assembly_file (Program function_def) =
  let output_channel = open_out assembly_file in
  emit_function output_channel function_def;
  emit_stack_note output_channel;
  close_out output_channel
