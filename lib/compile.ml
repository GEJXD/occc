let compile stage src_file =
  let source = In_channel.with_open_text src_file In_channel.input_all in
  let tokens = Lexer.lexer source in
  if stage = Settings.Lex then ()
  else
    let ast = Parser.parser tokens in
    (* Debug: Print parsing results (AST) *)
    if !Settings.debug then
      Ast_print.print_program ast
    else ();
    if stage = Settings.Parse then ()
    else
      (* resolve identifiers *)
      let resolved_variable_ast = Resolve.resolve ast in
      (* resolve loops and break/continue statements *)
      let loop_labeling_ast = Label_loops.label_loops resolved_variable_ast in
      (* typecheck definitions and functioncalls with correct arguments *)
      let _ = Typecheck.typecheck loop_labeling_ast in
      if stage = Settings.Validate then ()
      else
        let tacky = Tacky_gen.tacky_gen loop_labeling_ast in
        Tacky_print.debug_print_tacky src_file tacky;
        if stage = Settings.Tacky then ()
        else
          let asm_ast = Codegen.codegen tacky in
          (if !Settings.debug then
             let prealloc_filename =
               Filename.chop_extension src_file ^ ".prealloc.debug.s"
             in
             Emit.emit prealloc_filename asm_ast);
          (* replace pseudoregisters with Stack operands *)
          let asm_ast1 = Replace_pseudos.replace_pseudos asm_ast in
          (* fix up instructions *)
          let asm_ast2 = Instruction_fixup.fixup_program asm_ast1 in
          if stage = Settings.Codegen then ()
          else
            let asm_filename = Filename.chop_extension src_file ^ ".s" in
            Emit.emit asm_filename asm_ast2
