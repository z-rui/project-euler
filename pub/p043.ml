(** Sub-string Divisibility *)

let special_pandigitals () =
  let result = ref [] in
  let primes = [| 2; 3; 5; 7; 11; 13; 17 |] in
  let rec aux acc len used =
    if len < 4 || acc mod 1000 mod primes.(len - 4) = 0 then
      if len = 10 then result := acc :: !result
      else
        for d = 0 to 9 do
          if used land (1 lsl d) = 0 then
            let acc' = (acc * 10) + d in
            let used' = used lxor (1 lsl d) in
            aux acc' (len + 1) used'
        done
  in
  for d = 1 to 9 do
    aux d 1 (1 lsl d)
  done;
  !result

let () =
  special_pandigitals () |> List.fold_left ( + ) 0 |> print_int;
  print_newline ()
