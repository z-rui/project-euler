(** Square Root Digial Expansion *)

let sqrt_digit_sum len n =
  let x = Z.(sqrt (n * pow ~$100 len)) in
  String.to_seq (Z.to_string x)
  |> Seq.take len
  |> Seq.fold_left (fun acc c -> acc + (Char.code c - Char.code '0')) 0

let () =
  Seq.init 100 (fun i -> Z.of_int (i + 1))
  |> Seq.filter (Fun.negate Z.perfect_square)
  |> Seq.map (sqrt_digit_sum 100)
  |> Seq.fold_left ( + ) 0 |> print_int;
  print_newline ()
