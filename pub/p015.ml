let lattice_path_count rows cols = Z.bin (Z.of_int (rows + cols)) rows

let () =
  lattice_path_count 20 20 |> Z.print;
  print_newline ()
