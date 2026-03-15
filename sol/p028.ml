(** Number Spiral Diagonals *)

(* - Sequence: 1, 3, 5, 7, 9, 13, 17, 21, 25, ...
 * - Difference: 2, 2, 2, 2, 4, 4, 4, 4, ... 
 *)

let spiral_diagonals : int Seq.t =
  let rec next x d i =
    Seq.Cons
      (x, fun () -> next (x + d) (if i mod 4 = 0 then d + 2 else d) (i + 1))
  in
  fun () -> next 1 2 1

let () =
  let side_len = 1001 in
  let seq_len = (2 * side_len) - 1 in
  spiral_diagonals |> Seq.take seq_len |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
