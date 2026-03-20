(** Lychrel Numbers *)

let reverse s =
  let n = String.length s in
  String.init n @@ fun i -> s.[n - i - 1]

let is_palindrome s =
  let rec aux i j = i >= j || (s.[i] = s.[j] && aux (i + 1) (j - 1)) in
  aux 0 (String.length s - 1)

let probably_lychrel fuel n =
  let rec aux fuel n s =
    fuel = 0
    ||
    let n' = reverse s |> Z.of_string |> Z.add n in
    let s' = Z.to_string n' in
    (not (is_palindrome s')) && aux (fuel - 1) n' s'
  in
  let n = Z.of_int n in
  aux fuel n (Z.to_string n)

let () =
  Seq.ints 1 |> Seq.take 9999
  |> Seq.filter (probably_lychrel 50)
  |> Seq.length |> print_int;
  print_newline ()
