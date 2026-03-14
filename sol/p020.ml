let () =
  Z.fac 100 |> Euler.z_digits |> Seq.fold_left ( + ) 0 |> Printf.printf "%d\n"
