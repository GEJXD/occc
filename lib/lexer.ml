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
  matching_rule : match_rule; (* which rule it matched *)
  matched_substring : string; (* substring matched with regex token_match.re *)
}

(* convert string to Tokens.t *)
(* tips 1: treat keywords like special identifiers *)
let convert_identifier = function
  | "int" -> T.KWInt
  | "return" -> T.KWReturn
  | "void" -> T.KWVoid
  | other -> T.Identifier other

(* convert string to int *)
let convert_constant str = T.Constant (int_of_string str)

(* convert literal like "(" or ";" to Tokens.t *)
let convert_literal tok_type _s = tok_type

let match_rules = 
  let def re_str converter = 
    {re = Re.Pcre.regexp ~flags:[ `ANCHORED] re_str; converter}
  in [
    def {_|[A-Za-z_][A-Za-z0-9_]*\b|_} convert_identifier;
    (* constants *)
    def {_|[0-9]+\b|_} convert_constant;
    (* punctuation *)
    def {_|\(|_} (convert_literal T.OpenParen);
    def {_|\)|_} (convert_literal T.CloseParen);
    def {_|\{|_} (convert_literal T.OpenBrace);
    def {_|\}|_} (convert_literal T.CloseBrace);
    def ";" (convert_literal T.Semicolon);
  ]

let find_match s mat_rule = 
  let re = mat_rule.re in
  let try_match = Re.exec_opt re s in
  match try_match with
  | Some x -> Some {matched_substring=Re.Group.get x 0; matching_rule=mat_rule}
  | None -> None

let count_leading_ws s = 
  let ws_matcher = Re.Pcre.regexp ~flags:[ `ANCHORED ] {|\s+|} in
  let ws_match = Re.exec_opt ws_matcher s in
  match ws_match with
  | None -> None
  | Some x -> let _, match_end = Re.Group.offset x 0 in Some match_end

let convert_tokent_to_string = function
  | T.Identifier _ -> "Identifier"
  | T.Constant _ -> "Constant"
  | T.KWInt -> "Int"
  | T.KWReturn -> "Return"
  | T.KWVoid -> "Void"
  | _ -> "Characters" 

let rec print_list = function
  | [] -> print_endline ""
  | {matched_substring;matching_rule} :: t -> begin
    print_string (matching_rule.converter matched_substring
    |> convert_tokent_to_string);
    print_char ' ';
    print_list t;
  end

let rec lexer input = 
  if input = "" then []
  else
    match count_leading_ws input with
    (* have whitespace front of input *)
    | Some x -> lexer (StringUtil.drop x input)
    | None ->
    (* match_res contains all string that matched succesfully *)
      let match_result = List.filter_map (find_match input) match_rules in
      if match_result = [] then raise (LexError input)
      else begin
        print_list match_result;
        let matched_string = List.hd match_result in
        let converter = matched_string.matching_rule.converter in
        let matched_substring = matched_string.matched_substring in
        let next_tok = converter matched_substring in
        let remaining_input = StringUtil.drop (String.length matched_substring) input
      in next_tok :: lexer remaining_input
      end