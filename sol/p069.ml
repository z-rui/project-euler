(** Totient Maximum *)

(*
Note that
    Φ(n) = n * Π[p|n] (1-1/p).
Therefore, to maximize n/Φ(n), it suffices to minimize Π[p|n] (1-1/p).
That means n should be a product of distinct primes, and the smaller and the
more factors, the better.
*)

let () =
  let limit = Z.of_int 1_000_000 in
  let rec loop acc p =
    let acc' = Z.(acc * p) in
    if acc' > limit then acc else loop acc' (Z.nextprime p)
  in
  loop Z.one (Z.of_int 2) |> Z.print;
  print_newline ()
