
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

let dist x0 x1 =
  fabs (x0 -. x1)

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

let main () =
  let sizes = [1;2;4;8;16;32;64;128;256;512;1024;2048;4096;8192] in
  let query = one_rand_point_2D () in
  Printf.printf "#size vpt_t(s) brute_t(s)\n%!";
  List.iter (fun s ->
      let points = n_times s one_rand_point_2D in
      let vpt = Vp_tree.create dist_2D points in
      let vpt_delta_t, curr =
        time_it (fun () -> Vp_tree.nearest_neighbor dist_2D query vpt) in
      let brute_delta_t, reff =
        time_it (fun () -> brute_force_nearest_find dist_2D query points) in
      assert(curr = reff);
      Printf.printf "%d %f %f\n%!" s vpt_delta_t brute_delta_t
    ) sizes

let () = main ()
