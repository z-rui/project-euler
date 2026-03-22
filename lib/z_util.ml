let z_digits =
  Seq.unfold @@ fun z ->
  if Z.(equal zero z) then None
  else
    let q, r = Z.(div_rem z ~$10) in
    Some (Z.to_int r, q)
