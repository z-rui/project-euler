(** Large Non-Mersenne Prime *)

let () =
  let m = Z.of_int 10_000_000_000 in
  Z.(((~$28433 * powm ~$2 ~$7830457 m) + one) mod m |> print);
  print_newline ()
