(** Names Scores *)

let letter_value = function
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A' + 1
  | _ -> failwith "letter_value only works for A..Z"

let name_value s = String.fold_left (fun acc c -> acc + letter_value c) 0 s

let sum_name_scores names =
  let sum = ref 0 in
  List.iteri
    (fun i s -> sum := !sum + ((i + 1) * name_value s))
    (List.sort String.compare names);
  !sum

let parse_input input =
  In_channel.input_all input |> String.trim |> String.split_on_char ','
  |> List.map @@ fun s -> String.sub s 1 (String.length s - 2)

let () = parse_input stdin |> sum_name_scores |> Printf.printf "%d\n"
