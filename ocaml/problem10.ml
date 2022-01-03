(* This method takes hours to finish. *)

let check n primes =
  let root = sqrt (Int.to_float n) in
  match
    List.exists
      (fun prime -> Int.to_float prime <= root && n mod prime = 0)
      primes
  with
  | true -> primes
  | false -> List.cons n primes

let rec find_primes n max primes =
  if n >= max then primes
  else
    let next_primes = check n primes in
    find_primes (n + 1) max next_primes

(* Sieve takes a second to finish. *)

let rec sieve_remove_multiples arr n i max =
  if i > max then sieve_next_step arr (n + 2) max
  else
    let () = arr.(i) <- false in
    sieve_remove_multiples arr n (i + (2 * n)) max

and sieve_next_step arr n max =
  if Int.to_float n > sqrt (Int.to_float max) then arr
  else
    match arr.(n) with
    | false -> sieve_next_step arr (n + 2) max
    | true -> sieve_remove_multiples arr n (n + (2 * n)) max

let sieve max =
  let arr =
    Array.make max true
    (* Mark 0, 1, and multiples of 2 & 3 as false. *)
  in

  let () =
    arr.(0) <- false;
    arr.(1) <- false;

    Array.iteri
      (fun i x ->
        if i > 3 && (i mod 2 = 0 || i mod 3 = 0) then arr.(i) <- false)
      arr
    (* Perform the sieve. *)
  in

  sieve_next_step arr 5 max

let sum_sieve arr =
  Array.fold_left ( + ) 0
    (Array.mapi (fun i x -> if x = true then i else 0) arr)

let main () =
  let max = 2000000 in

  print_int (sum_sieve (sieve max));

  (*
    let primes = find_primes 4 max [3; 2]
    in Printf.printf "%d" (List.fold_left (+) 0 primes);
  *)
  exit 0
;;

main ()
