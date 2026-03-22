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

let quadratic n =
  let r, e = Z.sqrt_rem n in
  if Z.(equal zero e) then (r, [])
  else
    let initial = QZ.{ a = n; b = Z.neg r; d = Z.one } in
    let[@tail_mod_cons] rec loop w =
      let i, w' = QZ.reci w in
      if QZ.equal initial w' then [ i ] else i :: loop w'
    in
    (r, loop initial)

let convergent i f terms =
  let w = ref Q.zero in
  for i = terms - 1 downto 0 do
    w := Q.(one / (of_bigint (f i) + !w))
  done;
  Q.(of_bigint i + !w)
