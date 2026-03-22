let parse content =
  String.split_on_char ',' content
  |> List.map @@ fun s -> String.sub s 1 (String.length s - 2)

let letter_value = function
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A' + 1
  | _ -> failwith "letter_value only works for A..Z"

let word_value s = String.fold_left (fun acc c -> acc + letter_value c) 0 s
