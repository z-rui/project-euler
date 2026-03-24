(** Prime Power Triples *)

(* target < 50_000_000, and it is greater than the square power of the largest
   prime, so we need primes up to square root of 50_000_000 (≈ 7071). *)

let limit = 50_000_000
let primes = (Euler.Prime_table.create 7071).primes

let () =
  let module H = Hashtbl.Make (Int) in
  let seen =
    Bigarray.(Array1.create int8_unsigned c_layout ((limit + 7) / 8))
  in
  Bigarray.Array1.fill seen 0;
  let cnt = ref 0 in
  let rec loop_c i =
    let c = primes.(i) in
    let n1 = c * c * c * c in
    if n1 + 12 (* 2^2 + 2^3 *) <= limit then begin
      let rec loop_b j =
        let b = primes.(j) in
        let n2 = n1 + (b * b * b) in
        if n2 + 4 (* 2^2 *) <= limit then begin
          Array.iter
            begin fun a ->
              let n = n2 + (a * a) in
              if n < limit then
                (* bitmap reduces cache pressure *)
                let idx = n / 8 and bm = 1 lsl (n mod 8) in
                let bs = seen.{idx} in
                let bs' = bs lor bm in
                if bs <> bs' then begin
                  incr cnt;
                  seen.{idx} <- bs'
                end
            end
            primes;
          loop_b (j + 1)
        end
      in
      loop_b 0;
      loop_c (i + 1)
    end
  in
  loop_c 0;
  Printf.printf "%d\n" !cnt
