(** Counting Fractions *)

let () =
  let d_max = 1_000_000 in
  let phi = Euler.totient_table d_max in
  let cnt = ref 0 in
  for d = 2 to d_max do
    cnt := !cnt + phi.(d)
  done;
  Printf.printf "%d\n" !cnt
