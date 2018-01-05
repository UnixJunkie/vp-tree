
open Printf

let one_rand_point_2D () =
  (Random.float 1.0, Random.float 1.0)

let square x =
  x *. x

let dist_2D (x0, y0) (x1, y1) =
  sqrt (square (x0 -. x1) +. square (y0 -. y1))

let fabs x =
  if x > 0.0 then x
  else -.x

module Point_2D =
struct
  type t = float * float
  let dist = dist_2D
end

module Vpt = Vp_tree.Make(Point_2D)

let to_string_2D (x, y) =
  sprintf "%.3f %.3f" x y

let n_times n f =
  let res = ref [] in
  for _i = 1 to n do
    res := f() :: !res
  done;
  !res

let brute_force_nearest_find dist query points =
  let rec loop ((curr_d, curr_p) as acc) = function
    | [] -> acc
    | x :: xs ->
      let d = dist x query in
      let acc' = if d < curr_d then (d, x) else acc in
      loop acc' xs
  in
  match points with
  | [] -> assert(false)
  | p :: ps -> loop (dist query p, p) ps

let time_it f =
  let start = Unix.gettimeofday () in
  let res = f () in
  let stop = Unix.gettimeofday () in
  (stop -. start, res)

let query_several_times n vpt =
  let dt, _ =
    time_it (fun () ->
        for i = 1 to n do
          let q = one_rand_point_2D () in
          ignore(Vpt.nearest_neighbor q vpt)
        done
      ) in
  let q = one_rand_point_2D () in
  let res = Vpt.nearest_neighbor q vpt in
  (dt /. (float n), res, q)

let main () =
  (* test all neighbors within tolerance query *)
  let points = n_times 50_000 one_rand_point_2D in
  let tree = Vpt.create (Vpt.Good 50) points in
  let query = one_rand_point_2D () in
  let tol = Random.float 0.01 in
  let vpt_t, nearby_curr = time_it (fun () -> Vpt.neighbors query tol tree) in
  assert(List.for_all (fun p -> dist_2D query p <= tol) nearby_curr);
  let brute_t, nearby_ref = time_it (fun () ->
      List.filter (fun p -> dist_2D query p <= tol) points
    ) in
  assert(List.sort compare nearby_curr = List.sort compare nearby_ref);
  printf "#vpt_neighbors(%d): %f brute: %f accel: %.3f\n%!"
    (List.length nearby_curr)
    vpt_t brute_t (brute_t /. vpt_t);
  (* test nearest_neighbor queries *)
  let sizes = [1;2;4;8;16;32;64;128;256;512;1024;2048;4096;8192] in
  let ntimes = 100 in
  Printf.printf "#size b_c g_c r_c b_q g_q r_q brute\n";
  List.iter (fun size ->
      let points = n_times size one_rand_point_2D in
      (* create all VPTs *)
      let b_t, bvpt = time_it (fun () -> Vpt.create Vpt.Optimal points) in
      let g_t, gvpt = time_it (fun () -> Vpt.create (Vpt.Good 50) points) in
      let r_t, rvpt = time_it (fun () -> Vpt.create Vpt.Random points) in
      (* query all VPTs *)
      let bq_t, b_curr, q = query_several_times ntimes bvpt in
      let reff = brute_force_nearest_find dist_2D q points in
      assert(b_curr = reff);
      let gq_t, g_curr, q = query_several_times ntimes gvpt in
      let reff = brute_force_nearest_find dist_2D q points in
      assert(g_curr = reff);
      let rq_t, r_curr, q = query_several_times ntimes rvpt in
      let brute_t, reff =
        time_it (fun () -> brute_force_nearest_find dist_2D q points) in
      assert(r_curr = reff);
      Printf.printf "%d %f %f %f %f %f %f %f\n%!"
        size b_t g_t r_t bq_t gq_t rq_t brute_t
    ) sizes

let () = main ()
