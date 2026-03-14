let power_digit_sum b e =
  let rec loop acc x =
    if Z.(equal zero x) then acc
    else
      let q, r = Z.(div_rem x ~$10) in
      loop (acc + Z.to_int r) q
  in
  loop 0 (Z.pow (Z.of_int b) e)

let () = power_digit_sum 2 1000 |> Printf.printf "%d\n"
