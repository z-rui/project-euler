(** Distinct Powers *)

let distinct_powers a_max b_max =
  let rec aux acc a b =
    if b > b_max then if a = a_max then acc else aux acc (a + 1) 2
    else aux (Z.(~$a ** b) :: acc) a (b + 1)
  in
  List.sort_uniq Z.compare (aux [] 2 2)

let () = distinct_powers 100 100 |> List.length |> Printf.printf "%d\n"
