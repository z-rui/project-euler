(** Largest Exponential *)

let () =
  In_channel.(input_lines stdin)
  |> List.mapi (fun i line ->
      Scanf.sscanf line "%d,%d" @@ fun base expon ->
      (Float.(log (of_int base) *. of_int expon), i + 1))
  |> List.fold_left
       (fun ((x, _) as acc) ((y, _) as v) -> if x < y then v else acc)
       (Float.neg_infinity, 0)
  |> snd |> print_int;
  print_newline ()
