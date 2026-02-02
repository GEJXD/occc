open Ast
open Printf

let rec print_indent indent =
  if indent > 0 then (
    printf "  ";
    print_indent (indent - 1))

let print_unary_operator op =
  match op with Complement -> "~" | Negate -> "-" | Not -> "!"

let print_binary_operator op =
  match op with
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Mod -> "%"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | ShiftLeft -> "<<"
  | ShiftRight -> ">>"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "=="
  | NotEqual -> "!="
  | LessThan -> "<"
  | LessOrEqual -> "<="
  | GreaterThan -> ">"
  | GreaterOrEqual -> ">="

let print_compound_operator op =
  match op with
  | AddAssign -> "+="
  | SubtractAssign -> "-="
  | MultiplyAssign -> "*="
  | DivideAssign -> "/="
  | ModAssign -> "%="
  | BitAndAssign -> "&="
  | BitOrAssign -> "|="
  | BitXorAssign -> "^="
  | ShiftLeftAssign -> "<<="
  | ShiftRightAssign -> ">>="

let rec print_exp ?(indent = 0) exp =
  print_indent indent;
  match exp with
  | Constant n -> printf "Constant(%d)\n" n
  | Var name -> printf "Var(\"%s\")\n" name
  | Unary (op, operand) ->
      let op_str = print_unary_operator op in
      printf "Unary(%s, \n" op_str;
      print_exp ~indent:(indent + 1) operand;
      print_indent indent;
      printf ")\n"
  | Binary (op, left, right) ->
      let op_str = print_binary_operator op in
      printf "Binary(%s, \n" op_str;
      print_exp ~indent:(indent + 1) left;
      print_exp ~indent:(indent + 1) right;
      print_indent indent;
      printf ")\n"
  | Assignment (var, value) ->
      printf "Assignment(\n";
      print_exp ~indent:(indent + 1) var;
      print_exp ~indent:(indent + 1) value;
      print_indent indent;
      printf ")\n"
  | CompoundAssign (op, left, right) ->
      (* 新增：复合赋值节点 *)
      let op_str = print_compound_operator op in
      printf "CompoundAssign(%s, \n" op_str;
      print_exp ~indent:(indent + 1) left;
      print_exp ~indent:(indent + 1) right;
      print_indent indent;
      printf ")\n"

and print_statement ?(indent = 0) stmt =
  print_indent indent;
  match stmt with
  | Return exp ->
      printf "Return(\n";
      print_exp ~indent:(indent + 1) exp;
      print_indent indent;
      printf ")\n"
  | Expression exp ->
      printf "Expression(\n";
      print_exp ~indent:(indent + 1) exp;
      print_indent indent;
      printf ")\n"
  | Null -> printf "Null\n"

and print_declaration ?(indent = 0) decl =
  print_indent indent;
  match decl with
  | Declaration { name; init } ->
      printf "Declaration {\n";
      print_indent (indent + 1);
      printf "name = \"%s\";\n" name;
      print_indent (indent + 1);
      printf "init = ";
      (match init with
      | Some exp ->
          printf "\n";
          print_exp ~indent:(indent + 2) exp
      | None -> printf "None\n");
      print_indent indent;
      printf "}\n"

and print_block_item ?(indent = 0) item =
  print_indent indent;
  match item with
  | S stmt ->
      printf "S(\n";
      print_statement ~indent:(indent + 1) stmt;
      print_indent indent;
      printf ")\n"
  | D decl ->
      printf "D(\n";
      print_declaration ~indent:(indent + 1) decl;
      print_indent indent;
      printf ")\n"

and print_function_definition ?(indent = 0) func_def =
  print_indent indent;
  match func_def with
  | Function { name; body } ->
      printf "Function {\n";
      print_indent (indent + 1);
      printf "name = \"%s\";\n" name;
      print_indent (indent + 1);
      printf "body = [\n";
      List.iter (fun item -> print_block_item ~indent:(indent + 2) item) body;
      print_indent (indent + 1);
      printf "];\n";
      print_indent indent;
      printf "}\n"

let print_program prog =
  match prog with
  | Program func_def ->
      printf "Program(\n";
      print_function_definition ~indent:1 func_def;
      printf ")\n"

let rec print_exp_inline exp =
  match exp with
  | Constant n -> sprintf "%d" n
  | Var name -> sprintf "%s" name
  | Unary (op, operand) ->
      sprintf "(%s%s)" (print_unary_operator op) (print_exp_inline operand)
  | Binary (op, left, right) ->
      sprintf "(%s %s %s)" (print_exp_inline left) (print_binary_operator op)
        (print_exp_inline right)
  | Assignment (var, value) ->
      sprintf "(%s = %s)" (print_exp_inline var) (print_exp_inline value)
  | CompoundAssign (op, left, right) ->
      sprintf "(%s %s %s)" (print_exp_inline left)
        (print_compound_operator op)
        (print_exp_inline right)
