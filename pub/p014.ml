let longest_collatz_chain n =
  let memory = Array.make (n + 1) 0 in
  memory.(1) <- 1;
  let rec collatz_length acc x =
    if x mod 2 = 0 then collatz_length (acc + 1) (x / 2)
    else if x > n then collatz_length (acc + 2) (((3 * x) + 1) / 2)
    else
      match memory.(x) with
      | 0 ->
          let len = collatz_length 2 (((3 * x) + 1) / 2) in
          memory.(x) <- len;
          acc + len
      | len -> acc + len
  in
  Seq.ints 1
  |> Seq.map (fun x -> (collatz_length 0 x, x))
  |> Seq.take n
  |> Seq.fold_left max (0, 0)

let () =
  longest_collatz_chain 999_999 |> fun (n, k) -> Printf.printf "%d %d\n" n k
