(** Digit Fifth Powers *)

let pow x n =
  let rec aux acc = function 0 -> acc | n -> aux (acc * x) (n - 1) in
  aux 1 n

let digit_power_sum p n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (acc + pow (n mod 10) p) (n / 10)
  in
  aux 0 n

(* Number of n digits: lower bound = 10^(n-1)
 * 5th power of its digits: upper bound = 9^5 * n
 *
 * n 9^5 * n
 * 1   59049
 * 2  118098
 * 3  177147
 * 4  236196
 * 5  295245
 * 6  354294
 * 7  413343
 *
 * Thus no solution for n >= 7.
 *)

let () =
  let sum = ref 0 in
  for x = 2 to 999999 do
    if x = digit_power_sum 5 x then sum := !sum + x
  done;
  print_int !sum;
  print_newline ()
