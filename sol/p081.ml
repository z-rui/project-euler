(** Path Sum: Two Ways *)

let min_path_sum matrix rows cols =
  let dp = Array.make cols 0 in
  dp.(0) <- matrix.(0).(0);
  for j = 1 to cols - 1 do
    dp.(j) <- dp.(j - 1) + matrix.(0).(j)
  done;
  for i = 1 to rows - 1 do
    let row = matrix.(i) in
    dp.(0) <- dp.(0) + row.(0);
    for j = 1 to cols - 1 do
      dp.(j) <- Int.min dp.(j - 1) dp.(j) + row.(j)
    done
  done;
  dp.(cols - 1)

let () =
  let matrix = Euler.Matrix_file.parse int_of_string In_channel.stdin in
  min_path_sum matrix (Array.length matrix) (Array.length matrix.(0))
  |> print_int;
  print_newline ()
