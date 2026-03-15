(** 1000-digit Fibonacci Number *)

let brute_force d =
  let rec loop i x y =
    if Z.to_string x |> String.length >= d then i else loop (i + 1) y Z.(x + y)
  in
  Z.(loop 1 one one)

let math_trick d =
  (* Note: Fib(n) ≈ Φ^n / √5 = 10^(d-1) => n log10 Φ = d - 1 + ½ log10 5 *)
  let phi = 0.5 *. (Float.sqrt 5. +. 1.) in
  Float.((of_int d -. 1. +. (0.5 *. log10 5.)) /. log10 phi |> ceil |> to_int)

let () =
  let d = 1000 in
  let answer = math_trick d in
  assert (answer = brute_force d);
  Printf.printf "%d\n" answer
