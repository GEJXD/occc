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

(* ========== Expression Printing ========== *)

let rec print_exp ?(indent = 0) exp =
  print_indent indent;
  match exp with
  | Constant n -> printf "Constant(%d)\n" n
  | Var name -> printf "Var(\"%s\")\n" name
  | Unary (op, operand) ->
      let op_str = print_unary_operator op in
      printf "Unary(%s,\n" op_str;
      print_exp ~indent:(indent + 1) operand;
      print_indent indent;
      printf ")\n"
  | Binary (op, left, right) ->
      let op_str = print_binary_operator op in
      printf "Binary(%s,\n" op_str;
      print_exp ~indent:(indent + 1) left;
      print_exp ~indent:(indent + 1) right;
      print_indent indent;
      printf ")\n"
  | Assignment (lhs, rhs) ->
      printf "Assignment(\n";
      print_exp ~indent:(indent + 1) lhs;
      print_exp ~indent:(indent + 1) rhs;
      print_indent indent;
      printf ")\n"
  | CompoundAssign (op, lhs, rhs) ->
      let op_str = print_compound_operator op in
      printf "CompoundAssign(%s,\n" op_str;
      print_exp ~indent:(indent + 1) lhs;
      print_exp ~indent:(indent + 1) rhs;
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
  | FunCall { f; args } ->
      printf "FunctionCall {\n";
      print_indent (indent + 1);
      printf "f = \"%s\";\n" f;
      print_indent (indent + 1);
      printf "args = [\n";
      List.iter (fun arg -> print_exp ~indent:(indent + 2) arg) args;
      print_indent (indent + 1);
      printf "]\n";
      print_indent indent;
      printf "}\n"

(* ========== Statement Printing ========== *)

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
      | InitDecl { name; init } ->
          printf "\n";
          print_indent (indent + 2);
          printf "InitDecl { name = \"%s\"; init = " name;
          (match init with
          | Some e ->
              printf "\n";
              print_exp ~indent:(indent + 3) e;
              print_indent (indent + 2);
              printf "}"
          | None -> printf "None }");
          printf "\n"
      | InitExp opt_exp -> (
          match opt_exp with
          | Some e ->
              printf "\n";
              print_exp ~indent:(indent + 2) e
          | None -> printf "None\n"));
      print_indent (indent + 1);
      printf "condition = ";
      (match condition with
      | Some e ->
          printf "\n";
          print_exp ~indent:(indent + 2) e
      | None -> printf "None\n");
      print_indent (indent + 1);
      printf "post = ";
      (match post with
      | Some e ->
          printf "\n";
          print_exp ~indent:(indent + 2) e
      | None -> printf "None\n");
      print_indent (indent + 1);
      printf "body = \n";
      print_statement ~indent:(indent + 2) body;
      print_indent indent;
      printf "}\n"
  | Null -> printf "Null\n"

(* ========== Declaration & Block Printing ========== *)

and print_declaration ?(indent = 0) decl =
  print_indent indent;
  match decl with
  | VarDecl { name; init } ->
      printf "VarDecl {\n";
      print_indent (indent + 1);
      printf "name = \"%s\";\n" name;
      print_indent (indent + 1);
      printf "init = ";
      (match init with
      | Some e ->
          printf "\n";
          print_exp ~indent:(indent + 2) e
      | None -> printf "None\n");
      print_indent indent;
      printf "}\n"
  | FunDecl { name; params; body } ->
      printf "FunDecl {\n";
      print_indent (indent + 1);
      printf "name = \"%s\";\n" name;
      print_indent (indent + 1);
      printf "params = [%s];\n"
        (String.concat "; " (List.map (sprintf "\"%s\"") params));
      print_indent (indent + 1);
      printf "body = ";
      (match body with
      | Some b ->
          printf "\n";
          print_block ~indent:(indent + 2) b
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

(* ========== Program Printing ========== *)

let print_program prog =
  match prog with
  | Program funcs ->
      printf "Program([\n";
      List.iter (fun fd -> print_declaration ~indent:1 (FunDecl fd)) funcs;
      printf "])\n"

(* ========== Inline Printing (for debugging) ========== *)

let rec print_exp_inline exp =
  match exp with
  | Constant n -> sprintf "%d" n
  | Var name -> name
  | Unary (op, e) ->
      sprintf "(%s%s)" (print_unary_operator op) (print_exp_inline e)
  | Binary (op, l, r) ->
      sprintf "(%s %s %s)" (print_exp_inline l) (print_binary_operator op)
        (print_exp_inline r)
  | Assignment (l, r) ->
      sprintf "(%s = %s)" (print_exp_inline l) (print_exp_inline r)
  | CompoundAssign (op, l, r) ->
      sprintf "(%s %s %s)" (print_exp_inline l)
        (print_compound_operator op)
        (print_exp_inline r)
  | Conditional { condition; then_result; else_result } ->
      sprintf "(%s ? %s : %s)"
        (print_exp_inline condition)
        (print_exp_inline then_result)
        (print_exp_inline else_result)
  | FunCall { f; args } ->
      let args_str = String.concat ", " (List.map print_exp_inline args) in
      sprintf "%s(%s)" f args_str

let rec print_statement_inline stmt =
  match stmt with
  | Return e -> sprintf "return %s;" (print_exp_inline e)
  | Expression e -> sprintf "%s;" (print_exp_inline e)
  | If { condition; then_clause; else_clause } ->
      let then_str = print_statement_inline then_clause in
      let else_str =
        match else_clause with
        | Some e -> sprintf " else %s" (print_statement_inline e)
        | None -> ""
      in
      sprintf "if (%s) %s%s" (print_exp_inline condition) then_str else_str
  | Compound (Block items) ->
      let items_str =
        List.map
          (function
            | S s -> print_statement_inline s
            | D d -> (
                match d with
                | VarDecl { name; init = Some e } ->
                    sprintf "var %s = %s;" name (print_exp_inline e)
                | VarDecl { name; init = None } -> sprintf "var %s;" name
                | FunDecl _ -> "/* function declaration */"))
          items
      in
      sprintf "{ %s }" (String.concat " " items_str)
  | Break lbl -> sprintf "break %s;" lbl
  | Continue lbl -> sprintf "continue %s;" lbl
  | While { condition; body; id } ->
      sprintf "while (%s) %s /* id=%s */"
        (print_exp_inline condition)
        (print_statement_inline body)
        id
  | DoWhile { body; condition; id } ->
      sprintf "do %s while (%s); /* id=%s */"
        (print_statement_inline body)
        (print_exp_inline condition)
        id
  | For { init; condition; post; body; id } ->
      let init_str =
        match init with
        | InitDecl { name; init = Some e } ->
            sprintf "var %s = %s" name (print_exp_inline e)
        | InitDecl { name; init = None } -> sprintf "var %s" name
        | InitExp (Some e) -> print_exp_inline e
        | InitExp None -> ""
      in
      let cond_str =
        match condition with Some e -> print_exp_inline e | None -> ""
      in
      let post_str =
        match post with Some e -> print_exp_inline e | None -> ""
      in
      sprintf "for (%s; %s; %s) %s /* id=%s */" init_str cond_str post_str
        (print_statement_inline body)
        id
  | Null -> ";"
