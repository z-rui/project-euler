(** Distinct Powers *)

module ZSet = Set.Make (Z)

let distinct_powers a_max b_max =
  let rec aux acc a b =
    if b > b_max then if a = a_max then acc else aux acc (a + 1) 2
    else aux (ZSet.add Z.(~$a ** b) acc) a (b + 1)
  in
  aux ZSet.empty 2 2

let () = distinct_powers 100 100 |> ZSet.cardinal |> Printf.printf "%d\n"
