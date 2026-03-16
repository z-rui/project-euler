(** Sub-string Divisibility *)

let combine digits =
  Array.to_seq digits |> Seq.fold_left (fun acc d -> (acc * 10) + d) 0

let special_pandigitals () =
  let primes = [| 2; 3; 5; 7; 11; 13; 17 |] in
  let check digits =
    let rec aux curr i =
      if curr mod primes.(i) <> 0 then false
      else if i = 6 then true
      else aux (((curr * 10) + digits.(i + 4)) mod 1000) (i + 1)
    in
    aux ((((digits.(1) * 10) + digits.(2)) * 10) + digits.(3)) 0
  in
  (* a bit wasteful to enumerate all permutations, but easy to implement *)
  let digits = Array.init 10 Fun.id in
  digits.(0) <- 1;
  digits.(1) <- 0;
  let rec loop acc =
    let acc' = if check digits then acc + combine digits else acc in
    if Euler.next_permutation Int.compare digits then loop acc' else acc'
  in
  loop 0

let () =
  special_pandigitals () |> print_int;
  print_newline ()
