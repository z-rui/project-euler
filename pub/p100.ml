(** Arranged Probability *)

(* Let m out of n discs be blue the the other (n-m) red.
The probability requirement is
     m     m-1     1
    --- * ----- = ---
     n     n-1     2  
which is equivalent to 

    2(2m-1)^2 = (2n-1)^2 + 1

(Pell's equation.)  *)

let () =
  let rec loop x y limit =
    let ( + ), ( * ), ( / ) = Int64.(add, mul, div) in
    if (x + 1L) / 2L < limit then
      loop ((3L * x) + (4L * y)) ((3L * y) + (2L * x)) limit
    else Printf.printf "%Ld\n" ((y + 1L) / 2L)
  in
  loop 1L 1L 1_000_000_000_000L
