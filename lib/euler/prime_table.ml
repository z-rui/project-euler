type t = { is_prime : bool array; primes : int array }

let create n =
  (* sieve method *)
  let is_prime = Array.make (n + 1) true in
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  let cnt = ref 0 in
  let i = ref 2 in
  while !i * !i <= n do
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
  while !i <= n do
    if is_prime.(!i) then incr cnt;
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
