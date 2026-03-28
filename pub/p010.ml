let () =
  let primetab = Euler.Prime_table.create 1_999_999 in
  Array.fold_left ( + ) 0 primetab.primes |> Printf.printf "%d\n"
