(** Singular Integer Right Triangles *)

let () =
  Euler.Right_triangles.count_by_perimeter 1_500_000
  |> Array.fold_left (fun acc n -> if n = 1 then acc + 1 else acc) 0
  |> Printf.printf "%d\n"
