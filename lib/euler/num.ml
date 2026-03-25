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

let enumerate_divisors factors f =
  let rec aux divisor = function
    | [] -> f divisor
    | (p, n) :: rest ->
        let divisor' = ref divisor in
        for k = 0 to n - 1 do
          aux !divisor' rest;
          divisor' := !divisor' * p
        done;
        aux !divisor' rest
  in
  aux 1 factors

let sum_proper_divisors n =
  let sum = ref 0 in
  enumerate_divisors
    (factorize n |> List.of_seq)
    (fun d -> if d < n then sum := !sum + d);
  !sum

let factor_table n =
  let lpf = Array.make (n + 1) 0 in
  let i = ref 2 in
  while !i * !i <= n do
    if lpf.(!i) = 0 then begin
      let j = ref (!i * !i) in
      while !j <= n do
        if lpf.(!j) = 0 then lpf.(!j) <- !i;
        j := !j + !i
      done
    end;
    incr i
  done;
  let fac = Array.make (n + 1) [] in
  for i = 2 to n do
    fac.(i) <-
      begin match lpf.(i) with
      | 0 -> [ (i, 1) ]
      | p ->
          begin match fac.(i / p) with
          | (p', k) :: fs when p = p' -> (p', k + 1) :: fs
          | fs -> (p, 1) :: fs
          end
      end
  done;
  fac

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
