(** Quadratic Primes *)

let prime_table = Euler.Prime_table.create 1_000_000

let prime_length a b =
  let rec aux n =
    let x = ((n + a) * n) + b in
    if x > 0 && prime_table.is_prime.(x) then aux (n + 1) else n
  in
  aux 0

let () =
  let max_prime_len = ref 0 in
  let product = ref 0 in
  (* b must be prime for n=0 *)
  Array.to_seq prime_table.primes
  |> Seq.take_while (fun b -> b <= 1000)
  |> Seq.iter begin fun b ->
      for a = -999 to 999 do
        let len = prime_length a b in
        if len > !max_prime_len then begin
          max_prime_len := len;
          product := a * b
        end
      done
    end;
  Printf.printf "%d\n" !product
