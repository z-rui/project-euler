let () =
  let primetab = Euler.Prime_table.create 1_000_000 in
  Euler.Prime_table.nth_prime primetab 10000 |> Printf.printf "%d\n"
