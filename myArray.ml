(* extend the Array module *)

include Array

(* smaller array, without elt at index 'i' *)
let remove i a =
  let n = length a in
  assert(i >= 0 && i < n);
  let res = make (n - 1) (unsafe_get a 0) in
  let j = ref 0 in
  for i' = 0 to n - 1 do
    if i' <> i then
      (unsafe_set res !j (unsafe_get a i');
       incr j)
  done;
  res

(* <=> List.partition *)
let partition p a =
  let ok, ko =
    fold_right (fun x (ok_acc, ko_acc) ->
        if p x then (x :: ok_acc, ko_acc)
        else (ok_acc, x :: ko_acc)
      ) a ([], [])
  in
  (of_list ok, of_list ko)

(* <=> List.split *)
let split a =
  let n = length a in
  if n = 0 then ([||], [||])
  else
    let l, r = unsafe_get a 0 in
    let left = make n l in
    let right = make n r in
    for i = 1 to n - 1 do
      let l, r = unsafe_get a i in
      unsafe_set left i l;
      unsafe_set right i r
    done;
    (left, right)

(* <=> BatArray.min_max with default value in case of empty array *)
let min_max_def a def =
  let n = length a in
  if n = 0 then def
  else
    let mini = ref (unsafe_get a 0) in
    let maxi = ref (unsafe_get a 0) in
    for i = 1 to n - 1 do
      let x = unsafe_get a i in
      if x < !mini then mini := x;
      if x > !maxi then maxi := x
    done;
    (!mini, !maxi)

(* get one bootstrap sample of 'size' using sampling with replacement *)
let bootstrap_sample rng size a =
  let n = length a in
  assert(n > 0);
  assert(size < n);
  let res = make size (unsafe_get a 0) in
  for i = 0 to size - 1 do
    let rand = Random.State.int rng n in
    unsafe_set res i (unsafe_get a rand)
  done;
  res
