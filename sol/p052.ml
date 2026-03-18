(** Permuted Multiples *)

let hash n =
  let rec aux i mask = function
    | 0 -> (i, mask)
    | n -> aux (i + 1) (mask lor (1 lsl (n mod 10))) (n / 10)
  in
  aux 0 0 n

let () =
  match
    Seq.ints 1
    |> Seq.filter (fun n ->
        let h = hash (n + n) in
        Seq.init 4 (fun i -> (i + 3) * n |> hash)
        |> Seq.for_all (fun h' -> h = h'))
    |> Seq.uncons
  with
  | Some (n, _) ->
      print_int n;
      print_newline ()
  | None -> failwith "reaching the end of an infinity."
