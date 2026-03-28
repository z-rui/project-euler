(** Champernowne's Constant *)

(* n   # n-digit number   total length
 * 1                  9              9
 * 2                 90            189
 * 3                900           2889 
 * 4               9000          38889
 *)

let d n =
  let rec extract n = function
    | 0 -> n mod 10
    | i -> extract (n / 10) (i - 1)
  in
  let rec aux acc scale w =
    let acc' = acc + (scale * w) in
    if acc' >= n then
      let off = n - acc - 1 in
      let num = (scale / 9) + (off / w) and idx = w - 1 - (off mod w) in
      extract num idx
    else aux acc' (scale * 10) (w + 1)
  in
  aux 0 9 1

let () =
  List.map d [ 1; 10; 100; 1_000; 10_000; 100_000; 1_000_000 ]
  |> List.fold_left ( * ) 1 |> Printf.printf "%d\n"
