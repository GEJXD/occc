open Tacky

let pp_unary_operator out = function
  | Complement -> Format.pp_print_string out "~"
  | Negate -> Format.pp_print_string out "-"
  | Not -> Format.pp_print_string out "!"

let pp_binary_operator out = function
  | Add -> Format.pp_print_string out "+"
  | Subtract -> Format.pp_print_string out "-"
  | Multiply -> Format.pp_print_string out "*"
  | Divide -> Format.pp_print_string out "/"
  | Mod -> Format.pp_print_string out "%"
  | BitAnd -> Format.pp_print_string out "&"
  | BitOr -> Format.pp_print_string out "|"
  | BitXor -> Format.pp_print_string out "^"
  | ShiftLeft -> Format.pp_print_string out "<<"
  | ShiftRight -> Format.pp_print_string out ">>"
  | Equal -> Format.pp_print_string out "=="
  | NotEqual -> Format.pp_print_string out "!="
  | LessThan -> Format.pp_print_string out "<"
  | LessOrEqual -> Format.pp_print_string out "<="
  | GreaterThan -> Format.pp_print_string out ">"
  | GreaterOrEqual -> Format.pp_print_string out ">="

let pp_tacky_val out = function
  | Constant i -> Format.pp_print_int out i
  | Var s -> Format.pp_print_string out s

let pp_instruction out = function
  | Return v -> Format.fprintf out "Return(%a)" pp_tacky_val v
  | Unary { op; src; dst } ->
      Format.fprintf out "%a = %a%a" pp_tacky_val dst pp_unary_operator op
        pp_tacky_val src
  | Binary { op; src1; src2; dst } ->
      Format.fprintf out "%a = %a %a %a" pp_tacky_val dst pp_tacky_val src1
        pp_binary_operator op pp_tacky_val src2
  | Copy { src; dst } ->
      Format.fprintf out "%a = %a" pp_tacky_val dst pp_tacky_val src
  | Jump label -> Format.fprintf out "Jump(%s)" label
  | JumpIfZero (cond, label) ->
      Format.fprintf out "JumpIfZero(%a, %s)" pp_tacky_val cond label
  | JumpIfNotZero (cond, label) ->
      Format.fprintf out "JumpIfNotZero(%a, %s)" pp_tacky_val cond label
  | Label target ->
      Format.pp_print_break out 0 (-2);
      Format.fprintf out "%s:" target

let pp_function_definition out (Function { name; body }) =
  Format.pp_open_vbox out 0;
  Format.fprintf out "%s:" name;
  Format.pp_print_break out 0 4;
  Format.pp_open_vbox out 0;
  Format.(pp_print_list pp_instruction) out body;
  Format.pp_close_box out ();
  Format.pp_close_box out ();
  Format.pp_print_newline out () (* flush *)

let pp_program out (Program f) = pp_function_definition out f

let debug_print_tacky src_filename tacky_prog =
  if !Settings.debug then (
    let tacky_file = Filename.chop_extension src_filename ^ ".debug.tacky" in
    let chan = open_out tacky_file in
    let formatter = Format.formatter_of_out_channel chan in
    pp_program formatter tacky_prog;
    close_out chan)

module TackyPrinter = struct
  module T = struct
    include Tacky
  end

  let print_tacky_val = function
    | T.Constant c -> Printf.printf "%d" c
    | T.Var v -> Printf.printf "%%tmp_%s" v

  let print_unary_operator = function
    | T.Complement -> "~"
    | T.Negate -> "-"
    | T.Not -> "!"

  let print_binary_operator = function
    | T.Add -> "+"
    | T.Subtract -> "-"
    | T.Multiply -> "*"
    | T.Divide -> "/"
    | T.Mod -> "%"
    | T.BitAnd -> "&"
    | T.BitOr -> "|"
    | T.BitXor -> "^"
    | T.ShiftLeft -> "<<"
    | T.ShiftRight -> ">>"
    | T.Equal -> "=="
    | T.NotEqual -> "!="
    | LessThan -> "<"
    | LessOrEqual -> "<="
    | GreaterThan -> ">"
    | GreaterOrEqual -> ">="

  let print_instruction = function
    | T.Return value ->
        Printf.printf "  return ";
        print_tacky_val value;
        Printf.printf "\n"
    | T.Unary { op; src; dst } ->
        Printf.printf "  ";
        print_tacky_val dst;
        Printf.printf " = %s " (print_unary_operator op);
        print_tacky_val src;
        Printf.printf "\n"
    | T.Binary { op; src1; src2; dst } ->
        Printf.printf "  ";
        print_tacky_val dst;
        Printf.printf " = ";
        print_tacky_val src1;
        Printf.printf " %s " (print_binary_operator op);
        print_tacky_val src2;
        Printf.printf "\n"
    | Copy { src; dst } ->
        Printf.printf "  ";
        print_tacky_val dst;
        Printf.printf " = ";
        print_tacky_val src;
        Printf.printf "\n"
    | T.Jump label -> Printf.printf "  jump %s\n" label
    | T.JumpIfZero (value, label) ->
        Printf.printf "  if (";
        print_tacky_val value;
        Printf.printf ") goto %s\n" label
    | T.JumpIfNotZero (value, label) ->
        Printf.printf "  if (!";
        print_tacky_val value;
        Printf.printf ") goto %s\n" label
    | T.Label label -> Printf.printf "%s:\n" label

  let print_list instrs =
    Printf.printf "TACKY Instructions:\n";
    List.iter (fun instr -> print_instruction instr) instrs;
    Printf.printf "\n"
end
