(** Almost Equilateral Triangles *)

(* These triangles may be (a, a, a+1) or (a, a, a-1), where a > 1.

- Base: b=a+1 or b=a-1.
- Height: h^2 = a^2 - (b/2)^2.  (3a+1)(a-1)/4 or (3a-1)(a+1)/4.
- Area: S = b*h / 2

- If a is even, then 3a+1 and a-1 are odd,
  h = sqrt(odd) / 2 and area = odd * sqrt(odd) / 4.
  odd * sqrt(odd) is never divisible by 4.
  So this case is impossible.
- If a is odd, then 3a+1 and a-1 are even.
  (3a-1)(a+1) is divisble by 4, so h^2 is an integer.
  b is an even integer, so we would only need h^2 to be a perfect square
  for S to be an integer.

Since the base and height are always integers, we can simply enumerate all
right triangles (h, b/2, a) such that b=a+1 or a-1.
*)

let solve p_max =
  let sum = ref 0 in
  let[@inline] check a b =
    match b - a with 1 | -1 -> sum := !sum + (a + a + b) | _ -> ()
  in
  let rec loop_n n =
    let rec loop_m m =
      let a = (m * m) + (n * n) in
      if 3 * a < p_max then begin
        let b = 4 * m * n and b' = 2 * ((m * m) - (n * n)) in
        check a b;
        check a b';
        loop_m (m + 2)
      end
    in
    if n * n < p_max / 4 then begin
      loop_m (n + 1);
      loop_n (n + 1)
    end
  in
  loop_n 1;
  !sum

let () =
  solve 1_000_000_000 |> print_int;
  print_newline ()
