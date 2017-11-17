
module L = List

(* Vantage-point tree implementation
   Cf. "Data structures and algorithms for nearest neighbor search
   in general metric spaces" by Peter N. Yianilos for details.
   http://citeseerx.ist.psu.edu/viewdoc/\
   download?doi=10.1.1.41.4193&rep=rep1&type=pdf *)

(* Functorial interface *)

module type Point =
sig
  type t
  val dist: t -> t -> float
end

module Make = functor (P: Point) ->
struct

  type node = { vp: P.t;
                lb_low: float;
                lb_high: float;
                middle: float;
                rb_low: float;
                rb_high: float;
                left: t;
                right: t }
  and t = Empty
        | Node of node

  let new_node vp lb_low lb_high middle rb_low rb_high left right =
    Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right }

  type open_itv = { lbound: float; rbound: float }

  let new_open_itv lbound rbound =
    { lbound; rbound }

  let in_open_itv x { lbound ; rbound }  =
    (x > lbound) && (x < rbound)

  let square (x: float): float =
    x *. x

  let median (l: float list): float =
    let xs = Array.of_list l in
    Array.sort compare xs;
    let n = Array.length xs in
    if n mod 2 = 1 then
      xs.(n / 2)
    else
      0.5 *. (xs.(n / 2) +. xs.(n / 2 - 1))

  let distances (p: P.t) (points: P.t list) =
    L.rev_map (P.dist p) points

  let variance (mu: float) (xs: float list) =
    L.fold_left (fun acc x ->
        acc +. (square (x -. mu))
      ) 0.0 xs

  let remove (x: P.t) (l: P.t list) =
    let rec loop acc = function
      | [] -> raise Not_found
      | y :: ys ->
        if y = x then
          (y, L.rev_append acc ys)
        else
          loop (y :: acc) ys
    in
    loop [] l

  (* this is optimal and costly (O(n^2)); use random sampling
     if you are indexing many points with your vp-tree *)
  let select_vp (points: P.t list) =
    match points with
    | [] -> assert(false)
    | [x] -> (x, 0.0, [])
    | x :: xs ->
      let vp, mu, _spread, others =
        L.fold_left (fun (curr_p, curr_mu, curr_spread, curr_others) p ->
            let p, others = remove p points in
            let dists = distances p others in
            let mu = median dists in
            let spread = variance mu dists in
            if spread > curr_spread then (p, mu, spread, others)
            else (curr_p, curr_mu, curr_spread, curr_others)
          ) (x, 0.0, 0.0, xs) points
      in
      (vp, mu, others)

  exception Empty_list

  let min_max (l: float list): float * float =
    let rec loop ((mini, maxi) as acc) = function
      | [] -> acc
      | x :: xs -> loop (min x mini, max x maxi) xs
    in
    match l with
    | [] -> raise Empty_list
    | x :: xs -> loop (x, x) xs

  let rec create points =
    match points with
    | [] -> Empty
    | [x] -> new_node x 0. 0. 0. 0. 0. Empty Empty
    | _more ->
      let vp, mu, others = select_vp points in
      let dists = L.rev_map (fun p -> (P.dist vp p, p)) others in
      let lefties, righties = L.partition (fun (d, p) -> d < mu) dists in
      let ldists, lpoints = L.split lefties in
      let rdists, rpoints = L.split righties in
      let lb_low, lb_high =
        match ldists with
        | [] -> 0., 0.
        | _ -> min_max ldists in
      let rb_low, rb_high =
        match rdists with
        | [] -> 0., 0.
        | _ -> min_max rdists in
      let middle = (lb_high +. rb_low) *. 0.5 in
      new_node vp lb_low lb_high middle rb_low rb_high
        (create lpoints) (create rpoints)

  let rec find_nearest acc query tree =
    match tree with
    | Empty -> acc
    | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } ->
      let x = P.dist vp query in
      let tau, acc' =
        match acc with
        | None -> (x, Some (x, vp))
        | Some (tau, best) ->
          if x < tau then (x, Some (x, vp))
          else (tau, Some (tau, best))
      in
      let il = new_open_itv (lb_low -. tau) (lb_high +. tau) in
      let ir = new_open_itv (rb_low -. tau) (rb_high +. tau) in
      let in_il = in_open_itv x il in
      let in_ir = in_open_itv x ir in
      if x < middle then
        match in_il, in_ir with
        | false, false -> acc'
        | true, false -> find_nearest acc' query left
        | false, true -> find_nearest acc' query right
        | true, true ->
          (match find_nearest acc' query left with
           | None -> find_nearest acc' query right
           | Some (tau, best) ->
             match find_nearest acc' query right with
             | None -> Some (tau, best)
             | Some (tau', best') ->
               if tau' < tau then Some (tau', best')
               else Some (tau, best))
      else (* x >= middle *)
        match in_ir, in_il with
        | false, false -> acc'
        | true, false -> find_nearest acc' query right
        | false, true -> find_nearest acc' query left
        | true, true ->
          (match find_nearest acc' query right with
           | None -> find_nearest acc' query left
           | Some (tau, best) ->
             match find_nearest acc' query left with
             | None -> Some (tau, best)
             | Some (tau', best') ->
               if tau' < tau then Some (tau', best')
               else Some (tau, best))

  let nearest_neighbor query tree =
    match find_nearest None query tree with
    | None -> raise Not_found
    | Some (tau, best) -> (tau, best)

  let rec to_list = function
    | Empty -> []
    | Node { vp; lb_low; lb_high; middle; rb_low; rb_high; left; right } ->
      let lefties = to_list left in
      let righties = to_list right in
      L.rev_append lefties (vp :: righties)

  let is_empty = function
    | Empty -> true
    | Node _ -> false

end
