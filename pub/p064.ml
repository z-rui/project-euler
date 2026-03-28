let () =
  Seq.ints 1 |> Seq.take 10000
  |> Seq.filter (fun n ->
      let _, f = Euler.Continued_fraction.quadratic (Z.of_int n) in
      List.length f mod 2 = 1)
  |> Seq.length |> Printf.printf "%d\n"
