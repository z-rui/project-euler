let () =
  Z.(~$2 ** 1000)
  |> Euler.z_digits |> Seq.fold_left ( + ) 0 |> Printf.printf "%d\n"
