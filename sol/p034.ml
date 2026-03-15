(** Digit Fractorials *)

(* For n-th digit, the sum of fractorials of its digits is no greater than 9! * n
 * n  9! * n
 * 1  362800
 * 2  725760
 * 3 1088640
 * 4 1451520
 * 5 1814400
 * 6 2177280
 * 7 2540160
 * 8 2903040
 *
 * Therefore, so solution for n >= 8
 *)

let digit_fractorials () =
  let sum = ref 0 in
  let frac = Array.make 10 1 in
  for i = 1 to 9 do
    frac.(i) <- frac.(i - 1) * i
  done;
  let rec digit_frac_sum acc = function
    | 0 -> acc
    | n -> digit_frac_sum (acc + frac.(n mod 10)) (n / 10)
  in
  for i = 10 to frac.(9) * 7 do
    if digit_frac_sum 0 i = i then sum := !sum + i
  done;
  !sum

let () =
  digit_fractorials () |> print_int;
  print_newline ()
