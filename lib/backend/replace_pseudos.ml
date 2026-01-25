open Assembly

(* map mothods: add lookup find note that functional data structure are
   persistent. *)
module StringMap = Map.Make (String)

(* a state variant to represent the current state, for more functional, I prefer
   do not use ref keywords *)
type replacement_state = {
  current_offset : int; (* last used stack cell *)
  offset_map : int StringMap.t (* a map from int to string(pseudoregister) *);
}

let replace_operand state = function
  | Pseudo id -> begin
      match StringMap.find_opt id state.offset_map with
      (* already have assigned this identifier, return Assembly Stack operand *)
      | Some offset -> (state, Stack offset)
      (* have not assgiend it *)
      | None ->
          let new_offset = state.current_offset - 4 in
          let new_map = StringMap.add id new_offset state.offset_map in
          let new_state =
            { current_offset = new_offset; offset_map = new_map }
          in
          (new_state, Stack new_offset)
    end
  | other -> (state, other)

let replace_pseudo_in_instruction state = function
  | Mov (src, dst) ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_mov = Mov (new_src, new_dst) in
      (state2, new_mov)
  | Unary (op, dst) ->
      let state1, new_dst = replace_operand state dst in
      let new_unary = Unary (op, new_dst) in
      (state1, new_unary)
  | AllocateStack _ ->
      failwith
        "Internal error: AllocateStack shouldn't be present at this point"
  | Binary { op; src; dst } ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_binary = Binary { op; src = new_src; dst = new_dst } in
      (state2, new_binary)
  | Idiv operand ->
      let state1, new_oper = replace_operand state operand in
      (state1, Idiv new_oper)
  | (Ret | Cdq) as other -> (state, other)

(* the pseudo are seperated by function scope *)
let replace_pseudo_in_function (Function { name; instructions }) =
  let init_state = { current_offset = 0; offset_map = StringMap.empty } in
  let final_state, fixed_instructions =
    List.fold_left_map replace_pseudo_in_instruction init_state instructions
  in
  ( Function { name; instructions = fixed_instructions },
    final_state.current_offset )

let replace_pseudos (Program func_def) =
  let fixed_def, last_stack_cell = replace_pseudo_in_function func_def in
  (Program fixed_def, last_stack_cell)
