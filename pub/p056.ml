(** Powerful Digit Sum *)

let pow_digit_sum a b = Euler.Z_util.digit_sum Z.(~$a ** b)

let () =
  let max_sum = ref 0 in
  for a = 1 to 99 do
    for b = 1 to 99 do
      max_sum := Int.max !max_sum (pow_digit_sum a b)
    done
  done;
  Printf.printf "%d\n" !max_sum
