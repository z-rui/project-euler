(** Square Digit Chains *)

let digits_square_sum n =
  let rec aux acc = function
    | 0 -> acc
    | n ->
        let d = n mod 10 in
        aux (acc + (d * d)) (n / 10)
  in
  aux 0 n

let count89 max_log10 =
  let precompute_max = 9 * 9 * max_log10 in
  let cycle = Array.make (precompute_max + 1) 0 in
  cycle.(1) <- 1;
  cycle.(89) <- 89;
  let rec precompute n =
    match cycle.(n) with
    | 0 ->
        let x = precompute (digits_square_sum n) in
        cycle.(n) <- x;
        x
    | x -> x
  in
  for i = 2 to precompute_max do
    ignore (precompute i)
  done;
  let cnt = ref 0 in
  let rec loop acc i =
    if i < max_log10 then
      for d = 0 to 9 do
        loop (acc + (d * d)) (i + 1)
      done
    else if cycle.(acc) = 89 then incr cnt
  in
  loop 0 0;
  !cnt

let () =
  count89 7 |> print_int;
  print_newline ()
