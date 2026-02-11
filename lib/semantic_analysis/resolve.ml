open Ast
module StringMap = Map.Make (String)

type var_entry = { unique_name : string; from_current_block : bool }

let copy_variable_map var_map =
  StringMap.map (fun entry -> { entry with from_current_block = false }) var_map

let check_lvalue = function Var _ -> true | _ -> false

let rec resolve_exp var_map = function
  | Constant _ as c -> c
  | Assignment (left, right) ->
      if check_lvalue left then
        Assignment (resolve_exp var_map left, resolve_exp var_map right)
      else
        failwith
          (Format.asprintf
             "Expected expression on left-hand side of assignment statement, \
              found %a"
             pp_exp left)
  | Var id -> begin
      try Var (StringMap.find id var_map).unique_name
      with Not_found -> failwith (Printf.sprintf "Undeclared variable %s" id)
    end
  | Unary (op, e) -> Unary (op, resolve_exp var_map e)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp var_map e1, resolve_exp var_map e2)
  | Conditional { condition; then_result; else_result } ->
      Conditional
        {
          condition = resolve_exp var_map condition;
          then_result = resolve_exp var_map then_result;
          else_result = resolve_exp var_map else_result;
        }
  | CompoundAssign (op, left, right) ->
      if check_lvalue left then
        CompoundAssign (op, resolve_exp var_map left, resolve_exp var_map right)
      else
        failwith
          (Format.asprintf
             "Expected expression on left-hand side of assignment statement, \
              found %a"
             pp_exp left)

(* rename a to a.1 a.2 ... *)
let resolve_declaration var_map (Declaration { name; init }) =
  match StringMap.find_opt name var_map with
  | Some { from_current_block = true; _ } ->
      failwith "Duplicate variable declaration"
  | _ ->
      let unique_name = Unique_ids.make_named_temporary name in
      let new_map =
        StringMap.add name { unique_name; from_current_block = true } var_map
      in
      let resolved_init = Option.map (resolve_exp new_map) init in
      (new_map, Declaration { name = unique_name; init = resolved_init })

let rec resolve_statement var_map = function
  | Return exp -> Return (resolve_exp var_map exp)
  | Expression exp -> Expression (resolve_exp var_map exp)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = resolve_exp var_map condition;
          then_clause = resolve_statement var_map then_clause;
          else_clause = Option.map (resolve_statement var_map) else_clause;
        }
  | Compound block ->
      let new_variable_map = copy_variable_map var_map in
      Compound (resolve_block new_variable_map block)
  | Null -> Null

and resolve_block_item var_map = function
  | S s ->
      let resolved_s = resolve_statement var_map s in
      (var_map, S resolved_s)
  | D d ->
      let new_map, resolved_d = resolve_declaration var_map d in
      (new_map, D resolved_d)

and resolve_block var_map (Block items) =
  let _, resolved_items = List.fold_left_map resolve_block_item var_map items in
  Block resolved_items

let resolve_function_def (Function { name; body }) =
  let var_map = StringMap.empty in
  let resolved_body = resolve_block var_map body in
  Function { name; body = resolved_body }

let resolve (Program fn_def) = Program (resolve_function_def fn_def)
