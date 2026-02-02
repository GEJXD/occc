[@@@coverage exclude_file]

type t =
  (* tokens with contents *)
  | Identifier of string
  | Constant of int
  (* Keywords *)
  | KWInt
  | KWReturn
  | KWVoid
  (* punctuations *)
  | OpenParen
  | CloseParen
  | OpenBrace
  | CloseBrace
  | Semicolon
  (* unary operators *)
  | Tilde
  | Hyphen
  (* misc *)
  | DoubleHyphen
  (* binary operators *)
  | Plus
  | Star
  | Slash
  | Percent
  (* bitwise operators *)
  | Ampersand (* & *)
  | Pipe (* | *)
  | Caret (* ^ *)
  | LeftShift (* << *)
  | RightShift (* >> *)
  (* logical operator *)
  | Bang (* ! *)
  | LogicalAnd (* && *)
  | LogicalOr (* !! *)
  | DoubleEqual (* == *)
  | NotEqual (* != *)
  | LessThan (* < *)
  | GreaterThan (* > *)
  | LessOrEqual (* <= *)
  | GreaterOrEqual (* >= *)
  (* local variables *)
  | EqualSign (* = *)
  (* compound assignment *)
  | PlusEqual (* += *)
  | HyphenEqual (* -= *)
  | StarEqual (* *= *)
  | SlashEqual (* /= *)
  | PercentEqual (* %= *)
  | AmpersandEqual (* &= *)
  | PipeEqual (* |= *)
  | CaretEqual (* ^= *)
  | LeftShiftEqual (* <<= *)
  | RightShiftEqual (* >>= *)
[@@deriving show]
(* ppx extension, construct a t.pp *)
