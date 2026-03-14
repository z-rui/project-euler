let solve n =
  let rec loop acc i =
    if i = n then acc
    else if i mod 3 = 0 || i mod 5 = 0 then loop (acc + i) (i + 1)
    else loop acc (i + 1)
  in
  loop 0 1

let () =
  let answer = solve 1000 in
  Format.printf "%d\n" answer
