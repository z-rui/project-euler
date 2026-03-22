let () =
  let p_max = 1000 in
  let count = Euler.Right_triangles.count_by_perimeter p_max in
  Array.to_seq count |> Seq.mapi (fun i x -> (x, i)) |> Seq.fold_left max (0, -1)
  |> fun (cnt, i) -> Printf.printf "i=%d cnt=%d\n" i cnt
