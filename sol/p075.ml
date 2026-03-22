(** Singular Integer Right Triangles *)

let () =
  let p_max = 1_500_000 in
  let count = Euler.Right_triangles.count_by_perimeter p_max in
  Array.fold_left (fun acc n -> if n = 1 then acc + 1 else acc) 0 count
  |> Printf.printf "%d\n"
