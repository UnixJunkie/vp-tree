
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
  (** A vantage point tree. *)
  type t
  (** Quality of the constructed tree.
      Tree construction takes more time with higher quality.
      Tree query time takes less time with higher tree quality.
      If you have 100k or more points, use a Good or Random tree. *)
  type quality = Optimal
               | Good of int (* sample size *)
               | Random
  (** [create quality points]
      create a vantage point tree of given quality containing all points. *)
  val create: quality -> P.t list -> t
  (** [nearest_neighbor p vpt] return the distance along with the nearest
      neighbor to query point [p] in [vpt]. Warning: there may be several
      points at this distance from [p] in [vpt],
      but a single (arbitrary) one is returned. *)
  val nearest_neighbor: P.t -> t -> float * P.t
  (** [neighbors p tol vpt] return all points in [vpt] within
      [tol] distance from query point [p].
      I.e. all points returned are within [(d <= tol)] 
      distance from [p]. *)
  val neighbors: P.t -> float -> t -> P.t list
  (** [to_list vpt] return the list of points in [vpt]. *)
  val to_list: t -> P.t list
  (** [is_empty vpt] test if [vpt] is empty. *)
  val is_empty: t -> bool
end
