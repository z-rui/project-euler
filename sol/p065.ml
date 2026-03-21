(** Convergents of e *)

let convergent i f terms =
  let w = ref Q.zero in
  for i = terms - 1 downto 0 do
    w := Q.(one / (of_int (f i) + !w))
  done;
  Q.(of_int i + !w)

let () =
  convergent 2 (fun i -> if i mod 3 = 1 then ((i / 3) + 1) * 2 else 1) 99
  |> Q.num |> Euler.z_digits |> Seq.fold_left ( + ) 0 |> print_int;
  print_newline ()
