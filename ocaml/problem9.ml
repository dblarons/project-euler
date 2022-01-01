(* 334 < c < 1000 *)

(* 1 solution *)
(* 332 (a) + 333 (b) + 335 (c) = 1000 *)

(* 3 solutions *)
(* 329 + 335 + 336 = 1000 *)
(* 330 + 334 + 336 = 1000 *)
(* 331 + 333 + 336 = 1000 *)

(* 5 solutions *)
(* 327 + 336 + 337 = 1000 *)
(* 328 + 335 + 337 = 1000 *)
(* 329 + 334 + 337 = 1000 *)
(* 330 + 333 + 337 = 1000 *)
(* 331 + 332 + 337 = 1000 *)

(* 6 solutions *)
(* 325 + 337 + 338 = 1000 *)
(* 326 + 336 + 338 = 1000 *)
(* 327 + 335 + 338 = 1000 *)
(* 328 + 334 + 338 = 1000 *)
(* 329 + 333 + 338 = 1000 *)
(* 330 + 332 + 338 = 1000 *)

(* 8 solutions *)
(* 323 + 338 + 339 = 1000 *)
(* 324 + 337 + 339 = 1000 *)
(* 325 + 336 + 339 = 1000 *)
(* 326 + 335 + 339 = 1000 *)
(* 327 + 334 + 339 = 1000 *)
(* 328 + 333 + 339 = 1000 *)
(* 329 + 332 + 339 = 1000 *)
(* 330 + 331 + 339 = 1000 *)

(* 9 solutions *)

(* 11 solutions *)

(* etc. *)

(* 999 - 334 = 665 *)

(* number of solutions periscopes; less near c=338 and c=1000 and more in between *)

(*
   main function
     returns None when c >= 1000
     calls subroutine with c, c-1, and 1000 - c - (c-1)
     returns Some a * b * c when subroutine returns Some
     returns function with c+1
   subroutine
     returns None when b >= a
     returns Some [a, b, c] if a^2 + b^2 = c^2
     returns subroutine with c, c-2, and 1000 - c - (c - 2)
 *)

let rec test a b c =
  if a >= b then []
  else if (a * a) + (b * b) = (c * c) then [a; b; c]
  else test (a + 1) (b - 1) c

let rec triplet c =
  let b = c - 1 in
  let a = 1000 - c - b

  in if c >= 1000 then []
  else match test a b c with
  | [] -> triplet (c + 1)
  | xs -> xs;;

let main () =
  match triplet(335) with
  | [] -> Printf.printf "not found"
  | a::b::c::xs -> Printf.printf "a=%d b=%d c=%d a*b*c=%d\n" a b c (a * b * c)
  | otherwise -> Printf.printf "oops";

  exit 0;;

main ()
