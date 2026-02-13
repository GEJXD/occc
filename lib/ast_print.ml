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
      let op_str = print_compound_operator op in
      printf "CompoundAssign(%s, \n" op_str;
      print_exp ~indent:(indent + 1) left;
      print_exp ~indent:(indent + 1) right;
      print_indent indent;
      printf ")\n"
  | Conditional { condition; then_result; else_result } ->
      printf "Conditional {\n";
      print_indent (indent + 1);
      printf "condition = \n";
      print_exp ~indent:(indent + 2) condition;
      print_indent (indent + 1);
      printf "then_result = \n";
      print_exp ~indent:(indent + 2) then_result;
      print_indent (indent + 1);
      printf "else_result = \n";
      print_exp ~indent:(indent + 2) else_result;
      print_indent indent;
      printf "}\n"

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
  | If { condition; then_clause; else_clause } ->
      printf "If {\n";
      print_indent (indent + 1);
      printf "condition = \n";
      print_exp ~indent:(indent + 2) condition;
      print_indent (indent + 1);
      printf "then_clause = \n";
      print_statement ~indent:(indent + 2) then_clause;
      print_indent (indent + 1);
      printf "else_clause = ";
      (match else_clause with
      | Some else_stmt ->
          printf "\n";
          print_statement ~indent:(indent + 2) else_stmt
      | None -> printf "None\n");
      print_indent indent;
      printf "}\n"
  | Compound block ->
      printf "Compound(\n";
      print_block ~indent:(indent + 1) block;
      print_indent indent;
      printf ")\n"
  | Break label -> printf "Break(\"%s\")\n" label
  | Continue label -> printf "Continue(\"%s\")\n" label
  | While { condition; body; id } ->
      printf "While {\n";
      print_indent (indent + 1);
      printf "id = \"%s\";\n" id;
      print_indent (indent + 1);
      printf "condition = \n";
      print_exp ~indent:(indent + 2) condition;
      print_indent (indent + 1);
      printf "body = \n";
      print_statement ~indent:(indent + 2) body;
      print_indent indent;
      printf "}\n"
  | DoWhile { body; condition; id } ->
      printf "DoWhile {\n";
      print_indent (indent + 1);
      printf "id = \"%s\";\n" id;
      print_indent (indent + 1);
      printf "body = \n";
      print_statement ~indent:(indent + 2) body;
      print_indent (indent + 1);
      printf "condition = \n";
      print_exp ~indent:(indent + 2) condition;
      print_indent indent;
      printf "}\n"
  | For { init; condition; post; body; id } ->
      printf "For {\n";
      print_indent (indent + 1);
      printf "id = \"%s\";\n" id;
      print_indent (indent + 1);
      printf "init = ";
      (match init with
      | InitDecl decl ->
          printf "\n";
          print_declaration ~indent:(indent + 2) decl
      | InitExp opt_exp -> (
          match opt_exp with
          | Some exp ->
              printf "\n";
              print_exp ~indent:(indent + 2) exp
          | None -> printf "None\n"));
      print_indent (indent + 1);
      printf "condition = ";
      (match condition with
      | Some exp ->
          printf "\n";
          print_exp ~indent:(indent + 2) exp
      | None -> printf "None\n");
      print_indent (indent + 1);
      printf "post = ";
      (match post with
      | Some exp ->
          printf "\n";
          print_exp ~indent:(indent + 2) exp
      | None -> printf "None\n");
      print_indent (indent + 1);
      printf "body = \n";
      print_statement ~indent:(indent + 2) body;
      print_indent indent;
      printf "}\n"
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

and print_block ?(indent = 0) (Block items) =
  print_indent indent;
  printf "Block([\n";
  List.iter (fun item -> print_block_item ~indent:(indent + 1) item) items;
  print_indent indent;
  printf "])\n"

and print_function_definition ?(indent = 0) func_def =
  print_indent indent;
  match func_def with
  | Function { name; body } ->
      printf "Function {\n";
      print_indent (indent + 1);
      printf "name = \"%s\";\n" name;
      print_indent (indent + 1);
      printf "body = \n";
      print_block ~indent:(indent + 2) body;
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
  | Conditional { condition; then_result; else_result } ->
      sprintf "(%s ? %s : %s)"
        (print_exp_inline condition)
        (print_exp_inline then_result)
        (print_exp_inline else_result)

(* Inline printing functions for statements *)
let rec print_statement_inline stmt =
  match stmt with
  | Return exp -> sprintf "return %s;" (print_exp_inline exp)
  | Expression exp -> sprintf "%s;" (print_exp_inline exp)
  | If { condition; then_clause; else_clause } ->
      let then_str = print_statement_inline then_clause in
      let else_str =
        match else_clause with
        | Some else_stmt ->
            sprintf " else %s" (print_statement_inline else_stmt)
        | None -> ""
      in
      sprintf "if (%s) %s%s" (print_exp_inline condition) then_str else_str
  | Compound (Block items) ->
      let items_str =
        List.map
          (function
            | S stmt -> print_statement_inline stmt
            | D decl -> print_declaration_inline decl)
          items
      in
      sprintf "{ %s }" (String.concat " " items_str)
  | Break label -> sprintf "break %s;" label
  | Continue label -> sprintf "continue %s;" label
  | While { condition; body; id } ->
      sprintf "while (%s) %s id=%s"
        (print_exp_inline condition)
        (print_statement_inline body)
        id
  | DoWhile { body; condition; id } ->
      sprintf "do %s while (%s); id=%s"
        (print_statement_inline body)
        (print_exp_inline condition)
        id
  | For { init; condition; post; body; id } ->
      let init_str =
        match init with
        | InitDecl decl -> print_declaration_inline decl
        | InitExp opt_exp -> (
            match opt_exp with Some exp -> print_exp_inline exp | None -> "")
      in
      let cond_str =
        match condition with Some exp -> print_exp_inline exp | None -> ""
      in
      let post_str =
        match post with Some exp -> print_exp_inline exp | None -> ""
      in
      sprintf "for (%s; %s; %s) %s id=%s" init_str cond_str post_str
        (print_statement_inline body)
        id
  | Null -> ";"

and print_declaration_inline decl =
  match decl with
  | Declaration { name; init } ->
      let init_str =
        match init with
        | Some exp -> sprintf " = %s" (print_exp_inline exp)
        | None -> ""
      in
      sprintf "var %s%s" name init_str
