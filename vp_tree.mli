
(** Generic interface. *)

type 'a t

val create: ('a -> 'a -> float) -> 'a list -> 'a t
    
val nearest_neighbor: ('a -> 'a -> float) -> 'a -> 'a t -> float * 'a
                                                           
val to_list: 'a t -> 'a list
    
val is_empty: 'a t -> bool

(** Functorial interface. *)

module type Element =
sig

  type t   

  (** [dist] should be a distance function: symmetric, zero and the diagonal and verifying
      the triangular inequality.  *)
  val dist : t -> t -> float

end

module type S =
sig
  type t

  type elt

  val create: elt list -> t

  val nearest_neighbor: elt -> t -> float * elt

  val to_list: t -> elt list

  val is_empty: t -> bool
end

module Make : functor (E : Element) -> S with type elt = E.t
