open Ast
module StringMap = Map.Make (String)

let rec resolve_exp var_map = function
  | Assignment (left, right) ->
      let _ =
        match left with
        | Var _ -> ()
        | _ ->
            failwith
              (Format.asprintf
                 "Expected expression on left-hand side of assignment \
                  statement, found %a"
                 pp_exp left)
      in
      Assignment (resolve_exp var_map left, resolve_exp var_map right)
  | Var id -> begin
      try Var (StringMap.find id var_map)
      with Not_found -> failwith (Printf.sprintf "Undeclared variable %s" id)
    end
  | Unary (op, e) -> Unary (op, resolve_exp var_map e)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp var_map e1, resolve_exp var_map e2)
  | Constant _ as c -> c

let resolve_declaration var_map (Declaration { name; init }) =
  if StringMap.mem name var_map then failwith "Duplicate variable declaration"
  else
    let unique_name = Unique_ids.make_temporary () in
    let new_map = StringMap.add name unique_name var_map in
    let resolved_exp = Option.map (resolve_exp new_map) init in
    (new_map, Declaration { name = unique_name; init = resolved_exp })

let resolve_statement var_map = function
  | Return exp -> Return (resolve_exp var_map exp)
  | Expression exp -> Expression (resolve_exp var_map exp)
  | Null -> Null

let resolve_block_item var_map = function
  | S s ->
      let resolved_s = resolve_statement var_map s in
      (var_map, S resolved_s)
  | D d ->
      let new_map, resolved_d = resolve_declaration var_map d in
      (new_map, D resolved_d)

let resolve_function_def (Function { name; body }) =
  let var_map = StringMap.empty in
  let _, resolved_body = List.fold_left_map resolve_block_item var_map body in
  Function { name; body = resolved_body }

let resolve (Program fn_def) = Program (resolve_function_def fn_def)
