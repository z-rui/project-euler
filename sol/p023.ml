(** Non-Abundant Sums *)

let abundant_numbers : int Seq.t =
  Seq.ints 1 |> Seq.filter @@ fun n -> Euler.sum_proper_divisors n > n

let non_abundant_sums n =
  let a = Seq.take_while (fun x -> x < n) abundant_numbers |> Array.of_seq in
  let indicator = Array.init (n + 1) Fun.id in
  Array.iteri
    (fun i x ->
      let rec loop j =
        let k = x + a.(j) in
        if k <= n then begin
          indicator.(k) <- 0;
          loop (j + 1)
        end
      in
      loop i)
    a;
  Array.fold_left ( + ) 0 indicator

let () = non_abundant_sums 28123 |> Printf.printf "%d\n"
