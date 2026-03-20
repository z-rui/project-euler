let reciprocal_cycle d =
  let seen = Array.make d None in
  let rec aux n i =
    match n mod d with
    | 0 -> 0
    | r ->
        begin match seen.(r) with
        | Some i' -> i - i'
        | None ->
            seen.(r) <- Some i;
            aux (r * 10) (i + 1)
        end
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
