module IntHashtbl = Hashtbl.Make (Int)

let longest_collatz_chain n =
  let memory = IntHashtbl.create n in
  IntHashtbl.add memory 1 1;
  let rec collatz_length acc x =
    if x mod 2 = 0 then collatz_length (acc + 1) (x / 2)
    else
      match IntHashtbl.find_opt memory x with
      | Some len -> acc + len
      | None ->
          let len = collatz_length 2 (((3 * x) + 1) / 2) in
          IntHashtbl.add memory x len;
          acc + len
  in
  Seq.ints 1
  |> Seq.map (fun x -> (collatz_length 0 x, x))
  |> Seq.take n
  |> Seq.fold_left max (0, 0)

let () =
  longest_collatz_chain 999_999 |> fun (n, k) -> Printf.printf "%d %d\n" n k
