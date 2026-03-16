let is_triangle_num x =
  let two_x = Z.of_int (x + x) in
  let r = Z.sqrt two_x in
  Z.(equal two_x (r * (r + one)))

let () =
  In_channel.(input_all stdin)
  |> String.trim |> Euler.Word_file.parse |> List.to_seq
  |> Seq.map Euler.Word_file.word_value
  |> Seq.filter is_triangle_num |> Seq.length |> print_int;
  print_newline ()
