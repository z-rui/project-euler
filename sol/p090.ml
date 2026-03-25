(** Cube Digit Pairs *)

let bitmask =
  Array.init 10 (function 6 | 9 -> (1 lsl 6) lor (1 lsl 9) | d -> 1 lsl d)

let good_arrangement bs1 bs2 =
  let check d1 d2 =
    let b1 = bitmask.(d1) and b2 = bitmask.(d2) in
    (bs1 land b1 <> 0 && bs2 land b2 <> 0)
    || (bs1 land b2 <> 0 && bs2 land b1 <> 0)
  in
  check 0 1 && check 0 4 && check 0 9 && check 1 6 && check 2 5 && check 3 6
  && check 4 9 && check 6 4 && check 8 1

let solve () =
  let cnt = ref 0 in
  begin
    Euler.Combinatorics.bit_combinations 10 6 @@ fun cube1 ->
    Euler.Combinatorics.bit_combinations 10 6 @@ fun cube2 ->
    if cube1 <= cube2 && good_arrangement cube1 cube2 then incr cnt
  end;
  !cnt

let () =
  solve () |> print_int;
  print_newline ()
