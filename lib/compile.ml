let read_source src_file =
  In_channel.with_open_text src_file In_channel.input_all

let run_lex source = Lexer.lexer source

let run_parse tokens =
  let ast = Parser.parser tokens in
  if !Settings.debug then Ast_print.print_program ast;
  ast

let run_validate ast =
  let resolved = Resolve.resolve ast in
  let with_loops = Label_loops.label_loops resolved in
  let () = Typecheck.typecheck with_loops in
  with_loops

let run_tacky src_file ast =
  let tacky = Tacky_gen.tacky_gen ast in
  Tacky_print.debug_print_tacky src_file tacky;
  tacky

let run_codegen tacky =
  let asm_ast = Codegen.codegen tacky in
  let asm_ast1 = Replace_pseudos.replace_pseudos asm_ast in
  Instruction_fixup.fixup_program asm_ast1

let emit_assembly src_file asm =
  let asm_filename = Filename.chop_extension src_file ^ ".s" in
  Emit.emit asm_filename asm

let maybe_emit_debug_asm src_file asm =
  if !Settings.debug then
    let prealloc_filename =
      Filename.chop_extension src_file ^ ".prealloc.debug.s"
    in
    Emit.emit prealloc_filename asm

(* 管道状态：表示当前执行到哪一阶段及其结果 *)
type pipeline_state =
  | After_lex of Tokens.t list
  | After_parse of Ast.t
  | After_validate of Ast.t
  | After_tacky of Tacky.t
  | After_codegen of Assembly.t

let state_rank = function
  | After_lex _ -> 0
  | After_parse _ -> 1
  | After_validate _ -> 2
  | After_tacky _ -> 3
  | After_codegen _ -> 4

(* 目标 stage 需要推进到的最小 state 等级 *)
let required_rank = function
  | Settings.Lex -> 0
  | Settings.Parse -> 1
  | Settings.Validate -> 2
  | Settings.Tacky -> 3
  | Settings.Codegen | Settings.Assembly | Settings.Obj | Settings.Executable ->
      4

let advance src_file = function
  | After_lex tokens -> After_parse (run_parse tokens)
  | After_parse ast -> After_validate (run_validate ast)
  | After_validate ast -> After_tacky (run_tacky src_file ast)
  | After_tacky tacky -> After_codegen (run_codegen tacky)
  | After_codegen _ -> assert false

let rec run_until target src_file state =
  if state_rank state >= required_rank target then state
  else run_until target src_file (advance src_file state)

let compile stage src_file =
  let state =
    run_until stage src_file
      (After_lex (run_lex (read_source src_file)))
  in
  match (state, stage) with
  | After_codegen asm, (Settings.Assembly | Settings.Obj | Settings.Executable)
    ->
      maybe_emit_debug_asm src_file asm;
      emit_assembly src_file asm
  | _ -> ()
