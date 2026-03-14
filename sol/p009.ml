let () =
  for a = 1 to 1000 do
    for b = a to 1000 - a - 1 do
      let c = 1000 - a - b in
      if (a * a) + (b * b) = c * c then Printf.printf "%d\n" (a * b * c)
    done
  done
