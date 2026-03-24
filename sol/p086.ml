(** Cuboid Route *)

(* For three sides (a, b, c), the potential shortest paths are

- L_a = sqrt(a^2 + (b+c)^2),
- L_b = sqrt(b^2 + (a+c)^2), and
- L_c = sqrt(c^2 + (a+b)^2).

Assuming a <= b <= c, it's easy to prove that L_a >= L_b >= L_c.
Therefore, we only need to count (a, b, c) such that L_c is an integer,
that is, (a+b)^2 + c^2 is a perfect square.  This means there exists an integer 
z, such that (a+b, c, z) are the sizes of a right triangle.

We can solve the problem by enumerating all right triangles whose sides fit
the constraints of a, b, and c.  *)

let count_paths side_max =
  let cnt = ref 0 in
  let twoside_max = 2 * side_max in
  let rec check a_plus_b c x y =
    if a_plus_b <= twoside_max && c <= side_max then begin
      (* a <= b <= c.
         b = a_plus_b - a <= c => a >= a_plus_b - c.
         a <= b = a_plus_b - a => a <= a_plus_b / 2.  *)
      let a_min = Int.max 1 (a_plus_b - c) and a_max = a_plus_b / 2 in
      if a_min <= a_max then cnt := !cnt + (a_max - a_min + 1);
      check (a_plus_b + x) (c + y) x y (* check next multple of x and y *)
    end
  in
  (* Use Euclid's formula to generate primitive triangles of sides no more
     than 2 * side_max.
     The two sides are: m^2-n^2 and 2*m*n, where the m and n are coprime
     and exactly one of them is even.
     Because m^2 - n^2 > 0, m > n >= 1, so m is at least 2.  Then, because
     2 * m * n <= 2 * side_max, we have n <= side_max / 2.  *)
  for n = 1 to side_max / 2 do
    let rec loop m =
      let x = 2 * m * n and y = (m * m) - (n * n) in
      if x <= twoside_max && y <= twoside_max then begin
        if Euler.Num.gcd m n = 1 then begin
          check x y x y;
          check y x y x
        end;
        loop (m + 2) (* skip numbers with same oddity as n *)
      end
    in
    loop (n + 1)
  done;
  !cnt

let rec bsearch pred lo hi =
  if lo = hi then lo
  else
    let mid = lo + ((hi - lo) / 2) in
    if pred mid then bsearch pred lo mid else bsearch pred (mid + 1) hi

let rec search pred lo hi =
  match bsearch pred lo hi with
  | i when i = hi -> search pred hi (hi * 2)
  | i -> i

let () =
  search (fun m -> count_paths m > 1_000_000) 100 1000 |> Printf.printf "%d\n"
