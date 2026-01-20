let compile stage src_file =
  let source = In_channel.with_open_text src_file In_channel.input_all in
  (* Lex it *)
  let tokens = Lexer.lexer source in
  if stage = Settings.Lex then ()
  else
    let ast = Parser.parser tokens in
    if stage = Settings.Parse then ()
    else
      let tacky = Tacky_gen.tacky_gen ast in
      Tacky_print.debug_print_tacky src_file tacky;
      if stage = Settings.Tacky then ()
      else
        let asm_ast = Codegen.codegen ast in
        if stage = Settings.Codegen then ()
        else
          let asm_filename = Filename.chop_extension src_file ^ ".s" in
          Emit.emit asm_filename asm_ast
