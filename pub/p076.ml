(** Counting Summations *)

let count_sums n =
  let dp = Array.make (n + 1) 0 in
  dp.(0) <- 1;
  for v = 1 to n - 1 do
    for i = v to n do
      dp.(i) <- dp.(i) + dp.(i - v)
    done
  done;
  dp.(n)

let () =
  count_sums 100 |> print_int;
  print_newline ()
