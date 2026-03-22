(** Totient Permutation *)

let compute_key n =
  let w = ref n in
  let key = ref 0L in
  while !w <> 0 do
    let d = !w mod 10 in
    key := Int64.add !key (Int64.shift_left 1L (4 * d));
    w := !w / 10
  done;
  !key

let () =
  let limit = 10_000_000 in
  let tot = Euler.totient_table limit in
  Seq.ints 3
  |> Seq.take_while (fun x -> x < limit)
  |> Seq.fold_left
       (fun n n' ->
         if n' * tot.(n) < n * tot.(n') && compute_key n' = compute_key tot.(n')
         then n'
         else n)
       2
  |> print_int;
  print_newline ()
