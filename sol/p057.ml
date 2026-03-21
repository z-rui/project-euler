let () =
  let rec loop acc q = function
    | 0 -> acc
    | n ->
        let q' = Q.(one + (one / (one + q))) in
        let num, den = Q.(num q', den q') in
        let f x = Euler.z_digits x |> Seq.length in
        if f num > f den then loop (acc + 1) q' (n - 1) else loop acc q' (n - 1)
  in
  print_int (loop 0 Q.one 1000);
  print_newline ()
