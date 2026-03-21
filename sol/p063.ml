(** Powerful Digit Counts *)

(* b^n having n digits means 10^(n-1) <= b^n < 10^n.
   Taking base-10 logarithm,

       n - 1 <= n log10 b < n    or    1 - 1/n <= log10 b < 1.

   From log10 b < 1 we have b < 10.
   From 1 - 1/n <= log10 b we have 1/n >= 1 - log10 b or n <= 1/(1 - log10 b).
*)

let () =
  let cnt = ref 0 in
  for b = 1 to 9 do
    let n = 1. /. (1. -. Float.(log10 (of_int b))) in
    cnt := !cnt + Float.(to_int (floor n))
  done;
  print_int !cnt;
  print_newline ()
