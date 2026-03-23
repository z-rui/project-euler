let parse f chan =
  In_channel.input_lines chan
  |> List.map (fun line ->
      String.split_on_char ',' line |> Array.of_list |> Array.map f)
  |> Array.of_list
