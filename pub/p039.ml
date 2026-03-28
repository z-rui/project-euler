let () =
  Euler.Right_triangles.count_by_perimeter 1000
  |> Array.to_seq
  |> Seq.mapi (fun i x -> (x, i))
  |> Seq.fold_left max (0, -1)
  |> fun (cnt, i) -> Printf.printf "i=%d cnt=%d\n" i cnt
