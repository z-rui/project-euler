(** Representation of numbers in the form of (sqrt(A) + B / D), whereas A, B and
    D are integers. *)
module QZ = struct
  type t = { a : Z.t; b : Z.t; d : Z.t }

  let equal x y = Z.(equal x.a y.a && equal x.b y.b && equal x.d y.d)

  let reci { a; b; d } =
    let d' = Z.(a - (b * b)) in
    let d'' = Z.divexact d' d in
    assert (Z.(equal d' @@ (d'' * d)));
    let i = Z.((sqrt a - b) / d'') in
    let b' = Z.(-b - (i * d'')) in
    (i, { a; b = b'; d = d'' })
end

let continued_fraction_period n =
  let n = Z.of_int n in
  let r, e = Z.sqrt_rem n in
  if Z.(equal zero e) then 0
  else
    let initial = QZ.{ a = n; b = Z.neg r; d = Z.one } in
    let rec loop cnt w =
      let _, w' = QZ.reci w and cnt' = cnt + 1 in
      if QZ.equal initial w' then cnt' else loop (cnt + 1) w'
    in
    loop 0 initial

let () =
  Seq.ints 1 |> Seq.take 10000
  |> Seq.filter (fun n -> continued_fraction_period n mod 2 = 1)
  |> Seq.length |> Printf.printf "%d\n"
