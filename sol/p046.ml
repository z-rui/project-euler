let prime_table = Euler.Prime_table.create 10_000

let rec search n =
  let rec check i =
    let diff = n - prime_table.primes.(i) in
    if diff < 0 then false
    else if Z.perfect_square (Z.of_int (diff / 2)) then true
    else check (i + 1)
  in
  if n >= Array.length prime_table.is_prime then
    failwith "run out of primes; extend the prime table."
  else if not (prime_table.is_prime.(n) || check 1) then n
  else search (n + 2)

let () =
  search 9 |> print_int;
  print_newline ()
