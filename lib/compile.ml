let compile stage src_file =
  (* read in the file - TODO use streams? *)
  let source = In_channel.with_open_text src_file In_channel.input_all in
  (* Lex it *)
  let tokens = Lexer.lexer source in
  if stage = Settings.Lex then ()
  else
    let _ = Parser.parser tokens in
    if stage = Settings.Parse then ()
    else ()
