(** Spiral Primes *)

let prime_table = Euler.Prime_table.create 100_000

let is_prime n =
  if n < Array.length prime_table.is_prime then prime_table.is_prime.(n)
  else
    let rec loop i =
      if i = Array.length prime_table.primes then
        failwith "run out of primes; extend the prime table.";
      let p = prime_table.primes.(i) in
      p * p > n || (n mod p <> 0 && loop (i + 1))
    in
    loop 0

let take_drop n seq =
  let rec aux acc seq = function
    | 0 -> finish acc seq
    | n ->
        begin match seq () with
        | Seq.Nil -> finish acc seq
        | Seq.Cons (x, seq') -> aux (x :: acc) seq' (n - 1)
        end
  and finish acc seq = (List.(rev acc), seq) in
  aux [] seq n

(* see p028 *)
let spiral_diagonals : int Seq.t =
  let rec next x d i =
    Seq.Cons
      (x, fun () -> next (x + d) (if i mod 4 = 0 then d + 2 else d) (i + 1))
  in
  fun () -> next 1 2 1

let () =
  let rec loop primes total seq =
    let numbers, seq' = take_drop 4 seq in
    let primes' =
      List.fold_left
        (fun acc x -> if is_prime x then acc + 1 else acc)
        primes numbers
    and total' = total + 4 in
    if primes' * 10 < total' then (total' + 1) / 2 else loop primes' total' seq'
  in
  loop 0 1 (Seq.drop 1 spiral_diagonals) |> print_int;
  print_newline ()
