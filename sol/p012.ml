let triangle_numbers : int Seq.t =
  let rec next acc i = Seq.Cons (acc, fun () -> next (acc + i) (i + 1)) in
  fun () -> next 1 2

let () =
  let divisor_count n =
    Seq.fold_left (fun acc (_, k) -> acc * (k + 1)) 1 (Euler.Num.factorize n)
  in
  match
    triangle_numbers
    |> Seq.drop_while (fun x -> divisor_count x <= 500)
    |> Seq.uncons
  with
  | Some (x, _) -> Printf.printf "%d\n" x
  | None -> failwith "impossible"
