(* Number theory helpers *)

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a / gcd a b * b

let factorize n =
  let rec next divisor n =
    if divisor > n then Seq.Nil
    else if n mod divisor = 0 then
      let rec loop ord n' =
        if n' mod divisor = 0 then loop (ord + 1) (n' / divisor)
        else Seq.Cons ((divisor, ord), fun () -> next (divisor + 2) n')
      in
      loop 1 (n / divisor)
    else next (divisor + 2) n
  in
  let rec loop ord n' =
    if n' mod 2 = 0 then loop (ord + 1) (n' / 2)
    else if ord > 0 then Seq.Cons ((2, ord), fun () -> next 3 n')
    else next 3 n'
  in
  fun () -> loop 0 n

let sum_proper_divisors n =
  let sum = ref 0 in
  let rec aux divisor = function
    | [] -> if divisor < n then sum := !sum + divisor
    | (p, n) :: rest ->
        let divisor' = ref divisor in
        for k = 0 to n - 1 do
          aux !divisor' rest;
          divisor' := !divisor' * p
        done;
        aux !divisor' rest
  in
  aux 1 (factorize n |> List.of_seq);
  !sum

let totient_table n =
  let tot = Array.make (n + 1) 0 in
  let primes = Dynarray.create () in
  tot.(1) <- 1;
  for i = 2 to n do
    if tot.(i) = 0 then begin
      tot.(i) <- i - 1;
      Dynarray.add_last primes i
    end;
    let rec loop j =
      if j < Dynarray.length primes then
        let p = Dynarray.get primes j in
        let ip = i * p in
        if ip <= n then
          match i mod p with
          | 0 -> tot.(ip) <- tot.(i) * p
          | _ ->
              tot.(ip) <- tot.(i) * (p - 1);
              loop (j + 1)
    in
    loop 0
  done;
  tot
