(** Convergents of e *)

let () =
  Euler.Continued_fraction.convergent (Z.of_int 2)
    (fun i -> if i mod 3 = 1 then Z.of_int (((i / 3) + 1) * 2) else Z.one)
    99
  |> Q.num |> Euler.z_digits |> Seq.fold_left ( + ) 0 |> print_int;
  print_newline ()
