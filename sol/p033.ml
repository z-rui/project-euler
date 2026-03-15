let digit_cancelling_fractions () =
  let result = ref [] in
  let check a b c d =
    if a < b && a * d = b * c then result := (c, d) :: !result
  in
  for a = 1 to 9 do
    for b = 1 to 9 do
      let ab = (a * 10) + b in
      for c = 1 to 9 do
        check ab ((a * 10) + c) b c;
        check ab ((b * 10) + c) a c;
        check ab ((c * 10) + a) b c;
        check ab ((c * 10) + b) a c
      done
    done
  done;
  !result

let frac_mul (a, b) (c, d) =
  let num = a * c and denum = b * d in
  let g = Euler.gcd num denum in
  (num / g, denum / g)

let () =
  digit_cancelling_fractions ()
  |> List.fold_left frac_mul (1, 1)
  |> snd |> print_int;
  print_newline ()
