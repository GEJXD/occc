(* Tentative: no initializer tentative variable Initial: variable definitions
   with an NoInitializer NoInitializer: extern variables *)
type initial_value = Tentative | Initial of int | NoInitializer

type identifier_attrs =
  | FunAttr of { defined : bool; global : bool; stack_frame_size : int }
  | StaticAttr of { init : initial_value; global : bool }
  | LocalAttr

(* stack_frame_size is the function's stack frame size, meanless with
   variables *)
type entry = { t : Types.t; attrs : identifier_attrs }

let (symbol_table : (string, entry) Hashtbl.t) = Hashtbl.create 20

let modify k f =
  let v = Hashtbl.find symbol_table k in
  let v' = f v in
  Hashtbl.replace symbol_table k v'

(* since global declarations can present much times, we use replace instead of
   add to avoid entry presents more than once *)
let add_automatic_var name ~t =
  Hashtbl.replace symbol_table name { t; attrs = LocalAttr }

let add_static_var name ~t ~init ~global =
  let var_attr = StaticAttr { init; global } in
  Hashtbl.replace symbol_table name { t; attrs = var_attr }

let add_fun name ~t ~defined ~global =
  let fun_attr = FunAttr { global; defined; stack_frame_size = 0 } in
  Hashtbl.replace symbol_table name { t; attrs = fun_attr }

let get name = Hashtbl.find symbol_table name
let get_opt name = Hashtbl.find_opt symbol_table name

let is_global name =
  match (get name).attrs with
  | LocalAttr -> false
  | StaticAttr { global; _ } -> global
  | FunAttr { global; _ } -> global

(* check for storage duration *)
let is_static name =
  try
    match (get name).attrs with
    | LocalAttr -> false
    | StaticAttr _ -> true
    | FunAttr _ ->
        failwith "Internal error: functions do not have storage duration"
  with Not_found -> false

let bindings () = Hashtbl.to_seq symbol_table |> List.of_seq
let is_defined = Hashtbl.mem symbol_table

(* management function stack frame size *)
let set_bytes_required name bytes_requires =
  let update_bytes = function
    | { t; attrs = FunAttr f } ->
        { t; attrs = FunAttr { f with stack_frame_size = bytes_requires } }
    | _ -> failwith "Internal error: not a function"
  in
  modify name update_bytes

let get_bytes_required name =
  match Hashtbl.find symbol_table name with
  | { attrs = FunAttr f; _ } -> f.stack_frame_size
  | _ -> failwith "Internal error: not a function"
