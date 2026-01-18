(** Functions for reading streams of tokens *)

type t

exception End_of_stream

val take_token : t -> Tokens.t

val peek : t -> Tokens.t

val is_empty : t -> bool

val of_list : Tokens.t list -> t
