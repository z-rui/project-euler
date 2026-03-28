(** Consecutive Prime Sum *)

let limit = 1_000_000
let prime_table = Euler.Prime_table.create limit

let cumsum a =
  let n = Array.length a in
  let s = Array.make n 0 in
  let acc = ref 0 in
  for i = 0 to n - 1 do
    acc := !acc + a.(i);
    s.(i) <- !acc
  done;
  s

let () =
  let prime_sums = cumsum prime_table.primes in
  let len = Array.length prime_sums in
  let rec loop_even n =
    (* even length - must start from 2 *)
    let s = prime_sums.(n - 1) in
    if s < limit && prime_table.is_prime.(s) then s else loop_odd 1 (n - 1)
  and loop_odd start n =
    (* odd length - can start from any odd prime *)
    if start + n > len then loop_even (n - 1)
    else
      let s = prime_sums.(start + n - 1) - prime_sums.(start - 1) in
      if s >= limit then loop_even (n - 1)
      else if prime_table.is_prime.(s) then s
      else loop_odd (start + 1) n
  in
  match Array.find_index (fun x -> x > limit) prime_sums with
  | Some n -> loop_even (n - (n land 1)) |> Printf.printf "%d\n"
  | None -> ()
