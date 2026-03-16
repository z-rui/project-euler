(** Triangular, Pentagonal, and Hexagonal *)

let[@inline] isqrt n = Float.(of_int n |> sqrt |> round |> to_int)

let is_pentagon x =
  let det = 1 + (24 * x) in
  let r = isqrt det in
  r * r = det && (r + 1) mod 6 = 0

let search n =
  (* Note: all hexagonal numbers are triangular numbers. *)
  let rec aux x d = if is_pentagon x then x else aux (x + d) (d + 4) in
  aux (n * (n + n - 1)) ((4 * n) + 1)

let () =
  search 144 |> print_int;
  print_newline ()
