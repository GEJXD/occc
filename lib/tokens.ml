[@@@coverage exclude_file]

type t =
  (* tokens with contents *)
  | Identifier of string
  | Constant of int
  (* Keywords *)
  | KWInt
  | KWReturn
  | KWVoid
  (* punctuation *)
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Semicolon
  (* unary operator *)
  | Tilde
  | Hyphen
  (* misc *)
  | DoubleHyphen
  (* binary operator *)
  | Plus
  | Star
  | Slash
  | Percent
[@@deriving show]
(* ppx extension, construct a t.pp *)
