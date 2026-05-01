(** A key that can be mapped to values of type ['a] in the context. *)
type 'a key = 'a Hmap.key

module type S = sig
  (** Get the current value for a given key, or [None] if no value was
      associated with the key in the context. *)
  val get : 'a key -> 'a option

  (** [with_binding k v cb] calls [cb()] in a context in which [k] is bound to
      [v]. This does not affect storage outside of [cb()]. *)
  val with_binding : 'a key -> 'a -> (unit -> 'r) -> 'r

  (** [without_binding k cb] calls [cb()] in a context where [k] has no binding
      (possibly shadowing the current binding of [k] if it exists). *)
  val without_binding : 'a key -> (unit -> 'b) -> 'b
end
