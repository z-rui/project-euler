let compare_frac (a, b) (c, d) = (a * d) - (b * c)

let pred_proper_frac (n', d') d_max =
  let best = ref (0, 1) in
  for d = 2 to d_max do
    let n = ((n' * d) - 1) / d' in
    let frac = (n, d) in
    if compare_frac frac !best > 0 then best := frac
  done;
  !best

let () =
  let n, d = pred_proper_frac (3, 7) 1_000_000 in
  Printf.printf "%d/%d\n" n d
