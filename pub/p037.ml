let is_prime = (Euler.Prime_table.create 1_000_000).is_prime

let rec right_truncatable_prime n =
  is_prime.(n) && (n < 10 || right_truncatable_prime (n / 10))

let truncatable_primes () =
  let result = ref [] in
  let rec aux scale n =
    for i = 1 to 9 do
      let n' = (i * scale) + n in
      if n' < Array.length is_prime && is_prime.(n') then begin
        if right_truncatable_prime n' then result := n' :: !result;
        aux (scale * 10) n'
      end
    done
  in
  aux 10 2;
  aux 10 3;
  aux 10 5;
  aux 10 7;
  !result

let () =
  let result = truncatable_primes () in
  (* This number is given by the problem. *)
  if List.length result <> 11 then
    failwith "not enough results; extend the prime table.";
  List.fold_left ( + ) 0 result |> print_int;
  print_newline ()
