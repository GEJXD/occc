type t = Tokens.t Stream.t

exception End_of_stream

let take_token tokens =
  try Stream.next tokens with Stream.Failure -> raise End_of_stream

let is_empty tokens =
  try Stream.empty tokens ; true with Stream.Failure -> false

(* look at the next token without removing int from the stream *)
let peek tokens =
  match Stream.peek tokens with
  | Some token ->
      token
  | None ->
      raise End_of_stream

let of_list = Stream.of_list
