
type 'a t

val create: ('a -> 'a -> float) -> 'a list -> 'a t

val nearest_neighbor: ('a -> 'a -> float) -> 'a -> 'a t -> float * 'a

val to_list: 'a t -> 'a list

val is_empty: 'a t -> bool
