(** Convergents of e *)

let () =
  Euler.Continued_fraction.convergent (Z.of_int 2)
    (fun i -> if i mod 3 = 1 then Z.of_int (((i / 3) + 1) * 2) else Z.one)
    99
  |> Q.num |> Euler.Z_util.digit_sum |> print_int;
  print_newline ()
