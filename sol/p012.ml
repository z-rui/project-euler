let triangle_numbers : int Seq.t =
  let rec next acc i = Seq.Cons (acc, fun () -> next (acc + i) (i + 1)) in
  fun () -> next 1 2

let () =
  let divisors_no_more_than k n =
    let prime_factors = Euler.factorize n in
    let cnt = Seq.fold_left (fun acc (_, k) -> acc * (k + 1)) 1 prime_factors in
    cnt <= k
  in
  match
    triangle_numbers |> Seq.drop_while (divisors_no_more_than 500) |> Seq.uncons
  with
  | Some (x, _) -> Printf.printf "%d\n" x
  | None -> failwith "impossible"
