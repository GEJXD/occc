open Ast
module StringMap = Map.Make (String)

type var_entry = {
  unique_name : string;
  from_current_block : bool; (* whether defined at the current scope *)
  has_linkage : bool; (* whether an identifier has external linkage *)
}

(* return a copy of the map with from_current_block set to false, since the new
   declaration can shadow the external declaration *)
let copy_identifier_map id_map =
  StringMap.map (fun entry -> { entry with from_current_block = false }) id_map

let check_lvalue = function Var _ -> true | _ -> false

let rec resolve_exp id_map = function
  | Constant _ as c -> c
  | Assignment (left, right) ->
      if check_lvalue left then
        Assignment (resolve_exp id_map left, resolve_exp id_map right)
      else
        failwith
          (Format.asprintf
             "Expected expression on left-hand side of assignment statement, \
              found %a"
             pp_exp left)
  | Var id -> begin
      (* id_map return a record, not a string *)
      try Var (StringMap.find id id_map).unique_name
      with Not_found -> failwith (Printf.sprintf "Undeclared variable %s" id)
    end
  | Unary (op, e) -> Unary (op, resolve_exp id_map e)
  | Binary (op, e1, e2) ->
      Binary (op, resolve_exp id_map e1, resolve_exp id_map e2)
  | Conditional { condition; then_result; else_result } ->
      Conditional
        {
          condition = resolve_exp id_map condition;
          then_result = resolve_exp id_map then_result;
          else_result = resolve_exp id_map else_result;
        }
  | CompoundAssign (op, left, right) ->
      if check_lvalue left then
        CompoundAssign (op, resolve_exp id_map left, resolve_exp id_map right)
      else
        failwith
          (Format.asprintf
             "Expected expression on left-hand side of assignment statement, \
              found %a"
             pp_exp left)
  (* a function in an expression must be declaraed before use, but need not
     defined.*)
  | FunCall { f; args } -> begin
      try
        (* in correct case, the fn_name are same as f. but it can help
           type-checker to check whether it is a function or a variable *)
        let fn_name = (StringMap.find f id_map).unique_name in
        FunCall { f = fn_name; args = List.map (resolve_exp id_map) args }
      with Not_found -> failwith "Undeclared function"
    end

let resolve_optional_exp id_map = Option.map (resolve_exp id_map)

(* add new local variable to current scope *)
let resolve_local_var_helper id_map name storage_class =
  let _ =
    begin match StringMap.find_opt name id_map with
    | Some { from_current_block = true; has_linkage; _ } ->
        (* variable is stored in the map and was defined in the current block *)
        if not (has_linkage && storage_class = Some Extern) then
          failwith "Duplicate variable declaration"
        else ()
    | _ -> ()
    end
  in
  let entry =
    if storage_class = Some Extern then
      { unique_name = name; from_current_block = true; has_linkage = true }
    else
      let unique_name = Unique_ids.make_named_temporary name in
      { unique_name; from_current_block = true; has_linkage = false }
  in
  let new_map = StringMap.add name entry id_map in
  (new_map, entry.unique_name)

let resolve_local_var_declaration id_map { name; init; storage_class } =
  let new_map, unique_name =
    resolve_local_var_helper id_map name storage_class
  in
  let resolved_init = Option.map (resolve_exp new_map) init in
  (new_map, { name = unique_name; init = resolved_init; storage_class })

let resolve_for_init id_map = function
  | InitExp e -> (id_map, InitExp (resolve_optional_exp id_map e))
  | InitDecl d ->
      let new_map, resolved_decl = resolve_local_var_declaration id_map d in
      (new_map, InitDecl resolved_decl)

let rec resolve_statement id_map = function
  | Return exp -> Return (resolve_exp id_map exp)
  | Expression exp -> Expression (resolve_exp id_map exp)
  | If { condition; then_clause; else_clause } ->
      If
        {
          condition = resolve_exp id_map condition;
          then_clause = resolve_statement id_map then_clause;
          else_clause = Option.map (resolve_statement id_map) else_clause;
        }
  | While { condition; body; id } ->
      While
        {
          condition = resolve_exp id_map condition;
          body = resolve_statement id_map body;
          id;
        }
  | DoWhile { body; condition; id } ->
      DoWhile
        {
          body = resolve_statement id_map body;
          condition = resolve_exp id_map condition;
          id;
        }
  | For { init; condition; post; body; id } ->
      let new_variable_map = copy_identifier_map id_map in
      let id_map2, resolved_init = resolve_for_init new_variable_map init in
      For
        {
          init = resolved_init;
          condition = resolve_optional_exp id_map2 condition;
          post = resolve_optional_exp id_map2 post;
          body = resolve_statement id_map2 body;
          id;
        }
  | Compound block ->
      let new_variable_map = copy_identifier_map id_map in
      Compound (resolve_block new_variable_map block)
  | (Null | Break _ | Continue _) as s -> s

and resolve_block_item id_map = function
  | S s ->
      let resolved_s = resolve_statement id_map s in
      (id_map, S resolved_s)
  | D d ->
      let new_map, resolved_d = resolve_local_declaration id_map d in
      (new_map, D resolved_d)

and resolve_block id_map (Block items) =
  let _, resolved_items = List.fold_left_map resolve_block_item id_map items in
  Block resolved_items

and resolve_local_declaration id_map = function
  | VarDecl var_id ->
      let new_map, resolved_var_id =
        resolve_local_var_declaration id_map var_id
      in
      (new_map, VarDecl resolved_var_id)
  | FunDecl { body = Some _; _ } ->
      failwith "nested function definitions are not allowed"
  | FunDecl { storage_class = Some Static; _ } ->
      failwith "static keyword not allowed on local function declaration"
  | FunDecl fd ->
      let new_map, resolved_fd = resolve_function_declaration id_map fd in
      (new_map, FunDecl resolved_fd)

and resolve_params id_map =
  let resolve_parms_helper new_map param_name =
    resolve_local_var_helper new_map param_name None
  in
  List.fold_left_map resolve_parms_helper id_map

and resolve_function_declaration id_map fn =
  match StringMap.find_opt fn.name id_map with
  (* declaraed as variable not function, which do not have external linkage *)
  | Some { from_current_block = true; has_linkage = false; _ } ->
      failwith "Duplicate declaration"
  | _ ->
      let new_entry =
        { unique_name = fn.name; from_current_block = true; has_linkage = true }
      in
      let new_map = StringMap.add fn.name new_entry id_map in
      let inner_map = copy_identifier_map new_map in
      let inner_map1, resolved_params = resolve_params inner_map fn.params in
      let resolved_body = Option.map (resolve_block inner_map1) fn.body in
      (new_map, { fn with params = resolved_params; body = resolved_body })

let resolve_file_scope_variable_declaration id_map
    ({ name; _ } as vd : Ast.variable_declaration) =
  let new_map =
    StringMap.add name
      { unique_name = name; from_current_block = true; has_linkage = true }
      id_map
  in
  (new_map, vd)

let resolve_global_declaration id_map = function
  | FunDecl fd ->
      let id_map1, fd = resolve_function_declaration id_map fd in
      (id_map1, FunDecl fd)
  | VarDecl vd ->
      let id_map1, resolved_vd =
        resolve_file_scope_variable_declaration id_map vd
      in
      (id_map1, VarDecl resolved_vd)

let resolve (Program decls) =
  let _, resolved_decls =
    List.fold_left_map resolve_global_declaration StringMap.empty decls
  in
  Program resolved_decls
