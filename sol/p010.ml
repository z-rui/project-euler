let () =
  let primetab = Euler.Prime_table.create 1_999_999 in
  let sum = ref 0 in
  for i = 0 to Euler.Prime_table.length primetab - 1 do
    sum := !sum + Euler.Prime_table.nth_prime primetab i
  done;
  Printf.printf "%d\n" !sum
