let reciprocal_cycle denom =
  let seen = Array.make denom None in
  let rec aux num i =
    if num = 0 then 0
    else if num >= denom then aux (num mod denom) (i + 1)
    else
      match seen.(num) with
      | Some i' -> i - i'
      | None ->
          seen.(num) <- Some i;
          aux (num * 10) i
  in
  aux 1 0

let () =
  Seq.ints 1 |> Seq.take 999
  |> Seq.fold_left
       (fun ((d, n) as acc) d' ->
         let n' = reciprocal_cycle d' in
         if n' > n then (d', n') else acc)
       (0, 0)
  |> fst |> print_int;
  print_newline ()
