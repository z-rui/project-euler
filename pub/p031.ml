let coin_sums coins sum =
  let dp = Array.make (sum + 1) 0 in
  dp.(0) <- 1;
  Array.iter
    (fun v ->
      for i = 0 to sum - v do
        dp.(i + v) <- dp.(i + v) + dp.(i)
      done)
    coins;
  dp.(sum)

let () =
  let coins = [| 1; 2; 5; 10; 20; 50; 100; 200 |] in
  coin_sums coins 200 |> print_int;
  print_newline ()
