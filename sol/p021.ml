let sum_proper_divisors n =
  let sum = ref 0 in
  let rec aux factors = function
    | [] ->
        let n' = List.fold_left ( * ) 1 factors in
        if n' < n then sum := !sum + n'
    | (p, n) :: rest ->
        let factor = ref 1 in
        for k = 0 to n do
          aux (!factor :: factors) rest;
          factor := !factor * p
        done
  in
  aux [] (Euler.factorize n |> List.of_seq);
  !sum

let sum_amicable_numbers n =
  Seq.ints 1
  |> Seq.take (n - 1)
  |> Seq.fold_left
       (fun acc a ->
         let b = sum_proper_divisors a in
         if a < b && a = sum_proper_divisors b then acc + a + b else acc)
       0

let () =
  (* proper_divisors 220; *)
  sum_amicable_numbers 10000 |> Printf.printf "%d\n"
