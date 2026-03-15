let is_palindrome s =
  let rec aux i j =
    if i >= j then true
    else if s.[i] <> s.[j] then false
    else aux (i + 1) (j - 1)
  in
  aux 0 (String.length s - 1)

let bin n =
  Seq.unfold (function 0 -> None | n -> Some (n land 1, n lsr 1)) n
  |> Seq.map (function 0 -> '0' | _ -> '1')
  |> String.of_seq

let is_double_base_palindrome n =
  is_palindrome (Int.to_string n) && is_palindrome (bin n)

let () =
  Seq.ints 0 |> Seq.take 1_000_000
  |> Seq.filter is_double_base_palindrome
  |> Seq.fold_left ( + ) 0 |> print_int;
  print_newline ()
