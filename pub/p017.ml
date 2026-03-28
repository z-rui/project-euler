let to_word = function
  | 19 -> "nineteen"
  | 18 -> "eighteen"
  | 17 -> "seventeen"
  | 16 -> "sixteen"
  | 15 -> "fifteen"
  | 14 -> "fourteen"
  | 13 -> "thirteen"
  | 12 -> "twelve"
  | 11 -> "eleven"
  | 10 -> "ten"
  | 9 -> "nine"
  | 8 -> "eight"
  | 7 -> "seven"
  | 6 -> "six"
  | 5 -> "five"
  | 4 -> "four"
  | 3 -> "three"
  | 2 -> "two"
  | 1 -> "one"
  | 0 -> "zero"
  | _ -> invalid_arg "to_word only supports 0..19"

let rec to_words = function
  | 1000 -> [ "one"; "thousand" ]
  | x when x > 1000 -> invalid_arg "to_words only supports upto 1000"
  | x when x >= 100 ->
      let tail = match x mod 100 with 0 -> [] | x' -> "and" :: to_words x' in
      to_word (x / 100) :: "hundred" :: tail
  | x when x >= 20 ->
      let tens =
        match x / 10 with
        | 2 -> "twenty"
        | 3 -> "thirty"
        | 4 -> "forty"
        | 5 -> "fifty"
        | 6 -> "sixty"
        | 7 -> "seventy"
        | 8 -> "eighty"
        | 9 -> "ninety"
        | _ -> failwith "impossible"
      in
      let ones = match x mod 10 with 0 -> [] | x' -> [ to_word x' ] in
      tens :: ones
  | x -> [ to_word x ]

let () =
  Seq.ints 1 |> Seq.take 1000 |> Seq.map to_words
  |> Seq.fold_left (List.fold_left (fun acc s -> acc + String.length s)) 0
  |> Printf.printf "%d\n"
