(** Names Scores *)

let sum_name_scores names =
  let sum = ref 0 in
  List.iteri
    (fun i s -> sum := !sum + ((i + 1) * Euler.Word_file.word_value s))
    (List.sort String.compare names);
  !sum

let () =
  In_channel.(input_all stdin)
  |> String.trim |> Euler.Word_file.parse |> sum_name_scores
  |> Printf.printf "%d\n"
