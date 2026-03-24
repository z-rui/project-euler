(** Counting Rectangles *)

(*
Let (a, b) be the sides of the rectangle.
Then the number of rectangles within the rectangular grid is T(a) * T(b),
where
    T(n) = 1 + 2 + ... + n = n*(n+1) / 2.
*)

let () =
  let target = 2_000_000 in
  let limit = Float.(sqrt (of_int target) *. 2.0 |> sqrt |> ceil |> to_int) in
  let best = ref (0, 0) in
  let best_abs_err = ref Int.max_int in
  let test a b area =
    let abs_err = Int.abs (area - target) in
    if abs_err < !best_abs_err then begin
      best := (a, b);
      best_abs_err := abs_err
    end
  in
  for a = 1 to limit do
    let ta = a * (a + 1) / 2 in
    let tb_approx = Float.(of_int target /. of_int ta) in
    let b = Float.(sqrt (tb_approx *. 2.0) |> to_int) in
    let tb = b * (b + 1) / 2 in
    let area = ta * tb in
    test a b area;
    if area < target then test a (b + 1) (area + (ta * (b + 1)))
    else if area > target then test a (b - 1) (area - (ta * b))
  done;
  let a, b = !best in
  let ta = a * (a + 1) / 2 and tb = b * (b + 1) / 2 in
  Printf.printf "a=%d, b=%d, area=%d, rectangle_count= %d * %d = %d\n" a b
    (a * b) ta tb (ta * tb)
