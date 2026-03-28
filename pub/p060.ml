(** Prime Pair Sets *)

let prime_table = Euler.Prime_table.create 1_000_000

let is_prime n =
  if n < Array.length prime_table.is_prime then prime_table.is_prime.(n)
  else
    match Z.probab_prime (Z.of_int n) 5 with
    | 0 -> false
    | 2 -> true
    | _ -> failwith "prime test inconclusive"

let is_prime_pair p q =
  let sp, sq = Int.(to_string p, to_string q) in
  is_prime (int_of_string (sp ^ sq)) && is_prime (int_of_string (sq ^ sp))

let solve n k =
  let peers = Array.make n Z.zero in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let p = prime_table.primes.(i) and q = prime_table.primes.(j) in
      if is_prime_pair p q then peers.(i) <- Z.(logor (one lsl j) peers.(i))
    done
  done;
  let solutions = ref [] in
  let rec search len acc allowed =
    if len = k then solutions := acc :: !solutions
    else if not Z.(equal zero allowed) then begin
      let i = Z.trailing_zeros allowed in
      let p = prime_table.primes.(i) in
      search (len + 1) (p :: acc) Z.(allowed land peers.(i));
      search len acc Z.(allowed lxor (one lsl i))
    end
  in
  (* Skip using 2; it's definitely not good *)
  search 0 [] Z.((one lsl n) - ~$2);
  !solutions

let () =
  match solve 1200 5 with
  | x :: xs ->
      let sum l = List.fold_left ( + ) 0 l in
      let solution, min_sum =
        List.fold_left
          (fun ((l, k) as acc) l' ->
            let k' = sum l' in
            if k' < k then (l', k') else acc)
          (x, sum x)
          xs
      in
      print_string "Solution:";
      List.iter (Printf.printf " %d") solution;
      print_newline ();
      Printf.printf "Sum: %d\n" min_sum
  | [] -> print_endline "solution not found"
