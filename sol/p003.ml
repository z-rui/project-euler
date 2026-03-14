let largest_prime_factor n =
  let rec loop divisor n =
    if divisor * divisor > n then n
    else if n mod divisor = 0 then loop divisor (n / divisor)
    else loop (divisor + 1) n
  in
  loop 2 n

let () =
  let n = 600851475143 in
  largest_prime_factor n |> Printf.printf "%d\n"
