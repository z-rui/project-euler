let prime_table = Euler.Prime_table.create 10_000

let goldbach n =
  let rec loop i =
    let diff = n - prime_table.primes.(i) in
    if diff < 0 then false
    else if diff mod 2 = 0 && Z.perfect_square (Z.of_int (diff / 2)) then true
    else loop (i + 1)
  in
  loop 0

let search () =
  let rec aux i =
    if i >= Array.length prime_table.is_prime then
      failwith "run out of primes; extend the prime table."
    else if not (prime_table.is_prime.(i) || goldbach i) then i
    else aux (i + 2)
  in
  aux 9

let () =
  search () |> print_int;
  print_newline ()
