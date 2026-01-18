open Utils

module T = struct
include Tokens
end

exception LexError of string

type match_rule = {
  re : Re.re; (* the regex string to match a token *)
  converter : string -> Tokens.t; 
  (* a handler which convert matched string to token *)
}

type matched_string = {
  matched_rule : match_rule; (* which rule it matched *)
  matched_substring : string; (* substring matched with regex token_match.re *)
}

(* convert string to Tokens.t *)
(* tips 1: treat keywords like special identifiers *)
let convert_identifier = function
  | "int"     ->   T.KWInt
  | "return"  ->   T.KWReturn
  | "void"    ->   T.KWVoid
  | other     ->   T.Identifier other

(* convert string to int *)
let convert_constant str = 
  try T.Constant (int_of_string str)
  with Failure _ -> raise (LexError ("Invalid constant: " ^ str))

(* convert literal like "(" or ";" to Tokens.t *)
let convert_literal tok_type _ = tok_type

let generate_rule regex_str converter = 
  {re = Re.Pcre.regexp ~flags:[ `ANCHORED ] regex_str; converter}

let match_rules = 
  [
    generate_rule {_|[A-Za-z_][A-Za-z0-9_]*\b|_}  convert_identifier;
    generate_rule {_|[0-9]+\b|_}                  convert_constant;
    generate_rule {_|\(|_}                        (convert_literal T.OpenParen);
    generate_rule {_|\)|_}                        (convert_literal T.CloseParen);
    generate_rule {_|\{|_}                        (convert_literal T.OpenBrace);
    generate_rule {_|\}|_}                        (convert_literal T.CloseBrace);
    generate_rule ";"                             (convert_literal T.Semicolon);
    generate_rule "-"                             (convert_literal T.Hyphen);
    generate_rule "--"                            (convert_literal T.DoubleHyphen);
    generate_rule "~"                             (convert_literal T.Tilde)
  ]

let find_match s rule = 
  match Re.exec_opt rule.re s with
  | Some group ->
      let substr = Re.Group.get group 0 in
      Some { matched_substring = substr; matched_rule = rule}
  | None -> None

let count_leading_ws s = 
  let ws_re = Re.Pcre.regexp ~flags:[ `ANCHORED ] {|\s+|} in
  match Re.exec_opt ws_re s with
  | Some group -> 
      let _, end_pos = Re.Group.offset group 0 in
      Some end_pos
  | None -> None

let token_to_string = function
  | T.Identifier _  -> "Identifier"
  | T.Constant _    -> "Constant"
  | T.KWInt         -> "Int"
  | T.KWReturn      -> "Return"
  | T.KWVoid        -> "Void"
  | T.Semicolon     -> ";"
  | T.OpenBrace     -> "{" 
  | T.CloseBrace    -> "}"
  | T.OpenParen     -> "("
  | T.CloseParen    -> ")"
  | T.Tilde         -> "~"
  | T.Hyphen        -> "-"
  | T.DoubleHyphen  -> "--"

let print_matches matches = 
  List.iter (fun m -> 
    print_string (m.matched_rule.converter m.matched_substring |> token_to_string);
    print_char ' ';
  ) matches;
  print_newline ()

(* main lexing function *)
let rec lexer input = 
  if input = "" then []
  else
    match count_leading_ws input with
    (* have whitespace front of input *)
    | Some x -> lexer (StringUtil.drop x input)
    | None ->
    (* match_result contains all matched_strings that matched succesfully *)
      let match_result = List.filter_map (find_match input) match_rules in
      match match_result with
      | [] -> raise (LexError 
        ("Unrecognized token at: " ^ String.sub input 0 (min 10 (String.length input))))
      | _ ->
          (* Optional: for debug *)
          print_matches match_result;
          let longest = 
            List.fold_left
              (fun acc m -> if String.length m.matched_substring > String.length acc.matched_substring then m else acc)
              (List.hd match_result)
              (List.tl match_result)
          in
          let token = longest.matched_rule.converter longest.matched_substring in
          let remaining = StringUtil.drop (String.length longest.matched_substring)
            input in
          token :: lexer remaining
