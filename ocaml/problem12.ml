(* Binary search doesn't work because divisor count goes up and down. *)

let count_divisors t =
  let max = Float.to_int (sqrt (Int.to_float t)) in
  List.fold_left
    (fun acc i -> if t mod i = 0 then acc + 2 else acc)
    0
    (List.init max (fun i -> i + 1))

let triangle_number t = List.fold_left ( + ) 0 (List.init t (fun i -> i + 1))

let rec highly_divisible_triangle_number target min max =
  if min = max then 0
  else
    let triangle = triangle_number min in
    if count_divisors triangle > target then min
    else highly_divisible_triangle_number target (min + 1) max

let main () =
  let number = highly_divisible_triangle_number 500 1 100000 in
  let triangle = triangle_number number in
  let divisors = count_divisors triangle in
  Printf.printf "n: %d | t: %d | d: %d" number triangle divisors;

  exit 0
;;

main ()
