(** Amicable Chains *)

let solve n_max =
  let sum_proper_divisors = Array.make (n_max + 1) 1 in
  for i = 2 to n_max / 2 do
    let j = ref (i * 2) in
    while !j <= n_max do
      sum_proper_divisors.(!j) <- sum_proper_divisors.(!j) + i;
      j := !j + i
    done
  done;
  let longest = ref 0 in
  let longest_start = ref 0 in
  let chain = Array.make (n_max + 1) 0 in
  let rec visit n depth =
    match chain.(n) with
    | -1 -> 0
    | 0 ->
        chain.(n) <- depth;
        let n' = sum_proper_divisors.(n) in
        let k = if n' <= n_max then visit n' (depth + 1) - 1 else 0 in
        if k > 0 && n < !longest_start then longest_start := n;
        chain.(n) <- -1;
        k
    | depth' ->
        let len = depth - depth' in
        if len <= !longest then 0
        else begin
          longest := len;
          longest_start := n;
          len
        end
  in
  for i = 1 to n_max do
    ignore (visit i 1)
  done;
  !longest_start

let () =
  solve 1_000_000 |> print_int;
  print_newline ()
