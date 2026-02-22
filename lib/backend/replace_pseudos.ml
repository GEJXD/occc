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
  | Binary { op; src; dst } ->
      let state1, new_src = replace_operand state src in
      let state2, new_dst = replace_operand state1 dst in
      let new_binary = Binary { op; src = new_src; dst = new_dst } in
      (state2, new_binary)
  | Cmp (op1, op2) ->
      let state1, new_op1 = replace_operand state op1 in
      let state2, new_op2 = replace_operand state1 op2 in
      let new_cmp = Cmp (new_op1, new_op2) in
      (state2, new_cmp)
  | Idiv operand ->
      let state1, new_oper = replace_operand state operand in
      (state1, Idiv new_oper)
  | SetCC (code, op) ->
      let state1, new_op = replace_operand state op in
      (state1, SetCC (code, new_op))
  | ( Ret | Cdq | Label _ | JmpCC _ | Jmp _ | DeallocateStack _
    | AllocateStack _ | Call _ ) as other ->
      (state, other)
  | Push op ->
      let state1, new_op = replace_operand state op in
      (state1, Push new_op)

(* the pseudo are seperated by function scope *)
let replace_pseudo_in_function (Function { name; instructions }) =
  let init_state = { current_offset = 0; offset_map = StringMap.empty } in
  let final_state, fixed_instructions =
    List.fold_left_map replace_pseudo_in_instruction init_state instructions
  in
  Symbols.set_bytes_required name final_state.current_offset;
  Function { name; instructions = fixed_instructions }

let replace_pseudos (Program fn_defs) =
  let fixed_defs = List.map replace_pseudo_in_function fn_defs in
  Program fixed_defs
