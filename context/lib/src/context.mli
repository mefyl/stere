(** Stack-based context.

    Forked from ocaml-opentelemetry without hard dependency to
    thread-local-storage so this is compatible with js_of_ocaml. *)

include module type of Context_intf

(** Set the global provider of stack-based context. *)
val set_provider : (module S) -> unit

(** Create a new fresh key, distinct from any previously created key. *)
val create_key : unit -> 'a key

(** Get the current value for a given key, or [None] if no value was associated
    with the key in the context. *)
val get : 'a key -> 'a option

(** [with_binding k v cb] calls [cb()] in a context in which [k] is bound to
    [v]. This does not affect storage outside of [cb()]. *)
val with_binding : 'a key -> 'a -> (unit -> 'r) -> 'r

(** [without_binding k cb] calls [cb()] in a context where [k] has no binding
    (possibly shadowing the current binding of [k] if it exists). *)
val without_binding : 'a key -> (unit -> 'b) -> 'b
