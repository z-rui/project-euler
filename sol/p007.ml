let () =
  let Euler.Prime_table.{ primes } = Euler.Prime_table.create 1_000_000 in
  Printf.printf "%d\n" primes.(10000)
