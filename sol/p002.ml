let fib_seq : int Seq.t =
  let rec next x y = Seq.Cons (x, fun () -> next y (x + y)) in
  fun () -> next 0 1

let () =
  let limit = 4_000_000 in
  fib_seq
  |> Seq.filter (fun x -> x mod 2 = 0)
  |> Seq.take_while (fun x -> x <= limit)
  |> Seq.fold_left ( + ) 0 |> Printf.printf "%d\n"
