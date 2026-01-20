module ListUtil : sig
  val max : ('a -> 'a -> int) -> 'a list -> 'a
  (** [max cmp lst] returns the maximum element in [lst], according to [cmp] *)
end

module StringUtil : sig
  val drop : int -> string -> string
end
