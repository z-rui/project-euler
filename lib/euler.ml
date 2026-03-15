let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a / gcd a b * b

module Prime_table = struct
  type t = { is_prime : bool array; primes : int array }

  let create n =
    (* sieve method *)
    let is_prime = Array.make (n + 1) true in
    is_prime.(0) <- false;
    is_prime.(1) <- false;
    let cnt = ref 0 in
    let i = ref 2 in
    while !i <= n do
      if is_prime.(!i) then begin
        incr cnt;
        let j = ref (!i * !i) in
        while !j <= n do
          is_prime.(!j) <- false;
          j := !j + !i
        done
      end;
      incr i
    done;
    let primes = Array.make !cnt 0 in
    i := 0;
    for k = 2 to n do
      if is_prime.(k) then begin
        primes.(!i) <- k;
        incr i
      end
    done;
    assert (!i = !cnt);
    { is_prime; primes }
end

let factorize n =
  let rec next divisor n =
    if divisor > n then Seq.Nil
    else if n mod divisor = 0 then
      let rec loop ord n' =
        if n' mod divisor = 0 then loop (ord + 1) (n' / divisor)
        else Seq.Cons ((divisor, ord), fun () -> next 2 n')
      in
      loop 1 (n / divisor)
    else next (divisor + 1 + (divisor land 1)) n
  in
  fun () -> next 2 n

let z_digits =
  Seq.unfold (fun z ->
      if Z.(equal zero z) then None
      else
        let q, r = Z.(div_rem z ~$10) in
        Some (Z.to_int r, q))

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

let next_permutation cmp a =
  let n = Array.length a in
  let swap i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  in
  let rec rev i j =
    if i < j then begin
      swap i j;
      rev (i + 1) (j - 1)
    end
  in
  let rec loop i =
    if i < 0 then false
    else if cmp a.(i + 1) a.(i) <= 0 then loop (i - 1)
    else begin
      let j =
        let rec loop' acc j =
          if j = n then acc
          else if cmp a.(i) a.(j) < 0 && cmp a.(j) a.(acc) < 0 then
            loop' j (j + 1)
          else loop' acc (j + 1)
        in
        loop' (i + 1) (i + 2)
      in
      swap i j;
      rev (i + 1) (n - 1);
      true
    end
  in
  loop (n - 2)
