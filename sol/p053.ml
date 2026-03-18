let () =
  let million = Z.of_int 1_000_000 in
  let cnt = ref 0 in
  let c = Array.make 101 Z.zero in
  c.(0) <- Z.one;
  for i = 1 to 100 do
    for j = i downto 1 do
      c.(j) <- Z.add c.(j) c.(j - 1);
      if c.(j) > million then incr cnt
    done
  done;
  print_int !cnt;
  print_newline ()
