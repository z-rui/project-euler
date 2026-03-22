(** Prime Summations *)

let limit = 100
let prime_table = Euler.Prime_table.create limit

let count_sums n =
  let dp = Array.make (n + 1) 0 in
  dp.(0) <- 1;
  Array.iter
    (fun p ->
      for i = p to n do
        dp.(i) <- dp.(i) + dp.(i - p)
      done)
    prime_table.primes;
  (* discount the single-term sums (doesn't affect result for this problem) *)
  Array.iter (fun p -> dp.(p) <- dp.(p) - 1) prime_table.primes;
  dp

let () =
  let sums = count_sums limit in
  match Array.find_index (fun x -> x > 5000) sums with
  | None -> failwith "no solution; extend search limit"
  | Some x ->
      Printf.printf "%d can be written as sum of primes in %d ways\n" x sums.(x)
