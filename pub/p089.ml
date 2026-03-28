(** Roman Numerals *)

let roman_char = function
  | 'I' -> 1
  | 'V' -> 5
  | 'X' -> 10
  | 'L' -> 50
  | 'C' -> 100
  | 'D' -> 500
  | 'M' -> 1000
  | _ -> invalid_arg "roman_char: bad char"

let roman_to_number s =
  let rec loop acc i last =
    let n = roman_char s.[i] in
    let acc' = if n < last then acc - n else acc + n in
    if i = 0 then acc' else loop acc' (i - 1) n
  in
  loop 0 (String.length s - 1) 0

let number_to_roman n =
  let open Buffer in
  let buf = create 8 in
  let rec aux ten five one = function
    | 9 ->
        add_char buf one;
        add_char buf ten
    | (8 | 7 | 6 | 5) as n ->
        add_char buf five;
        aux ten five one (n - 5)
    | 4 ->
        add_char buf one;
        add_char buf five
    | n ->
        for _ = 1 to n do
          add_char buf one
        done
  in
  for _ = 1 to n / 1000 do
    add_char buf 'M'
  done;
  aux 'M' 'D' 'C' (n mod 1000 / 100);
  aux 'C' 'L' 'X' (n mod 100 / 10);
  aux 'X' 'V' 'I' (n mod 10);
  contents buf

let () =
  In_channel.fold_lines
    begin fun acc s ->
      let n = roman_to_number s in
      let s' = number_to_roman n in
      acc + String.(length s - length s')
    end
    0 In_channel.stdin
  |> print_int;
  print_newline ()
