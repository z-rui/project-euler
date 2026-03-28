(** Lexicographic Permutations *)

let () =
  let arr = [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 |] in
  for i = 1 to 999_999 do
    if not (Euler.Combinatorics.next_permutation Int.compare arr) then
      failwith "impossible"
  done;
  Array.iter print_int arr;
  print_newline ()
