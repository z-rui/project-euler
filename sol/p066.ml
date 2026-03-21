(** Diophantine Equation *)

(*
Pell's equation: x^2 - Dy^2 = 1.
Solution is as follows: let the continued fraction of sqrt(D) be
    [b; (a(1), a(2), ..., a(r))]
where (a(1), ..., a(r)) is the repeating part, and a(r) = 2b.
Let m(k)/n(k) be the k-th convergents, then the fundamental solution is
  - x=m(r-1), y=n(r-1) if r even,
  - x=m(2*r-1), y=n(2*r-1) if r odd.
*)

let solve_pell_equation d =
  let i, f = Euler.Continued_fraction.quadratic (Z.of_int d) in
  let f = Array.of_list f in
  match Array.length f with
  | 0 -> None
  | r ->
      let sol =
        Euler.Continued_fraction.convergent i
          (fun i -> f.(i mod r))
          (if r mod 2 = 0 then r - 1 else r + r - 1)
      in
      Some Q.(num sol, den sol)

let () =
  Seq.ints 2
  |> Seq.take_while (fun x -> x <= 1000)
  |> Seq.fold_left
       (fun ((_, (x, _)) as acc) d' ->
         match solve_pell_equation d' with
         | Some ((x', _) as sol) when Z.gt x' x -> (d', sol)
         | _ -> acc)
       (0, (Z.one, Z.zero))
  |> fun (d, (x, y)) ->
  Printf.printf "D=%d\nx=%s\ny=%s\n" d (Z.to_string x) (Z.to_string y)
