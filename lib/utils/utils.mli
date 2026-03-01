module ListUtil : sig
  val max : ('a -> 'a -> int) -> 'a list -> 'a
  (** [max cmp lst] returns the maximum element in [lst], according to [cmp] *)

  val take_drop : int -> 'a list -> 'a list * 'a list
  (*[take_drop n lst] take the first [n] elements in [lst] and drop it *)
end

module StringUtil : sig
  val drop : int -> string -> string
end
