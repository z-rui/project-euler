let () =
  let lcm = ref 1 in
  for i = 1 to 20 do
    lcm := Euler.Num.lcm i !lcm
  done;
  Printf.printf "%d\n" !lcm
