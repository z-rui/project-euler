(** Counting Fractions in a Range *)

let n_approx_below (num, den) d = ((num * d) - 1) / den
let n_approx_above (num, den) d = (num * d / den) + 1

(* Semi-brute force.  This could be improved with Farey sequence. *)
let count_proper_frac lower upper d_max =
  let cnt = ref 0 in
  for d = 2 to d_max do
    let n_min = n_approx_above lower d and n_max = n_approx_below upper d in
    for n = n_min to n_max do
      if Euler.gcd n d = 1 then incr cnt
    done
  done;
  !cnt

let () =
  count_proper_frac (1, 3) (1, 2) 12_000 |> print_int;
  print_newline ()
