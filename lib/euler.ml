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
    Printf.eprintf "%d primes up to %d\n" !cnt n;
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
    else next (divisor + 1) n
  in
  fun () -> next 2 n
