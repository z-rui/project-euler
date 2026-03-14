module IntHashtbl = Hashtbl.Make (Int)

let longest_collatz_chain n =
  let memory = IntHashtbl.create n in
  IntHashtbl.add memory 1 1;
  let rec collatz_length x =
    let memorize x =
      match IntHashtbl.find_opt memory x with
      | Some len -> len
      | None ->
          let len = 1 + collatz_length ((3 * x) + 1) in
          IntHashtbl.add memory x len;
          len
    in
    let rec aux acc x =
      if x mod 2 = 0 then aux (acc + 1) (x / 2) else acc + memorize x
    in
    aux 0 x
  in
  Seq.ints 1
  |> Seq.map (fun x -> (collatz_length x, x))
  |> Seq.take n
  |> Seq.fold_left max (0, 0)

let () =
  longest_collatz_chain 999_999 |> fun (n, k) -> Printf.printf "%d %d\n" n k
