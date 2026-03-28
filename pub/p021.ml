let sum_amicable_numbers n =
  Seq.ints 1
  |> Seq.take (n - 1)
  |> Seq.fold_left
       (fun acc a ->
         let b = Euler.Num.sum_proper_divisors a in
         if a < b && a = Euler.Num.sum_proper_divisors b then acc + a + b
         else acc)
       0

let () =
  (* proper_divisors 220; *)
  sum_amicable_numbers 10000 |> Printf.printf "%d\n"
