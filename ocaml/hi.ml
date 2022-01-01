let check n primes =
  let root = sqrt(Int.to_float(n))
  in match List.exists (fun prime -> (Int.to_float(prime) <= root) && (n mod prime = 0)) primes with
  | true -> primes
  | false -> List.cons n primes

let rec find_primes n max primes =
  if n >= max then primes
  else let next_primes = check n primes
  in find_primes (n + 1) max next_primes

let main () =
  let primes = find_primes 4 200000 [3; 2]
  in Printf.printf "%d" (List.fold_left (+) 0 primes);

  (* in List.iter (Printf.printf "%d ") primes; *)

  exit 0;;

main ()
