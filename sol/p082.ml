(** Path Sum: Three Ways *)

let min_path_sum matrix rows cols =
  let dp = Array.make rows 0 in
  for i = 0 to rows - 1 do
    dp.(i) <- matrix.(i).(0)
  done;
  for j = 1 to cols - 1 do
    dp.(0) <- dp.(0) + matrix.(0).(j);
    for i = 1 to rows - 1 do
      dp.(i) <- Int.min dp.(i) dp.(i - 1) + matrix.(i).(j)
    done;
    for i = rows - 2 downto 0 do
      dp.(i) <- Int.min dp.(i) (dp.(i + 1) + matrix.(i).(j))
    done
  done;
  Array.fold_left Int.min Int.max_int dp

let () =
  let matrix = Euler.Matrix_file.parse int_of_string In_channel.stdin in
  min_path_sum matrix (Array.length matrix) (Array.length matrix.(0))
  |> print_int;
  print_newline ()
