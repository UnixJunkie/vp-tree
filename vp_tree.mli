
(** Functorial interface. *)

module type Point =
sig
  type t
  (** [dist] should be a distance function: symmetric, zero and
      the diagonal and verifying the triangular inequality. *)
  val dist: t -> t -> float
end

module Make: functor (P: Point) ->
sig
  type t
  val create: P.t list -> t
  val nearest_neighbor: P.t -> t -> float * P.t
  val to_list: t -> P.t list
  val is_empty: t -> bool
end
