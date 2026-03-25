(** Product-sum Numbers *)

let solve k_max =
  let n_max = k_max * 2 in
  let factors = Euler.Num.factor_table n_max in
  let enumerate_prod_sum n f =
    let rec aux prod sum len m min_d =
      Euler.Num.enumerate_divisors factors.(m) @@ fun d ->
      if d >= min_d then
        let prod' = prod * d and sum' = sum + d and len' = len + 1 in
        match m / d with
        | 1 -> f prod' sum' len'
        | m' -> aux prod' sum' len' m' d
    in
    aux 1 0 0 n 2
  in
  let min_prod_sum = Array.make (k_max + 1) Int.max_int in
  for n = 4 to n_max do
    enumerate_prod_sum n @@ fun prod sum len ->
    let k = prod - sum + len in
    if k <= k_max && min_prod_sum.(k) > n then min_prod_sum.(k) <- n
  done;
  let seen = Array.make (n_max + 1) false in
  let sum = ref 0 in
  for k = 2 to k_max do
    let n = min_prod_sum.(k) in
    if not seen.(n) then begin
      seen.(n) <- true;
      sum := !sum + n
    end
  done;
  !sum

let () =
  solve 12000 |> print_int;
  print_newline ()
