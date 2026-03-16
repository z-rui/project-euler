let powm_sum n m =
  Seq.ints 1 |> Seq.take n |> Seq.map Z.of_int
  |> Seq.fold_left (fun acc i -> Z.((acc + powm i i m) mod m)) Z.zero

let () =
  powm_sum 1000 Z.(~$10_000_000_000) |> Z.print;
  print_newline ()
