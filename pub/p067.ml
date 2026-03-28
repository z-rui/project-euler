(* see p018 *)
let max_path_sum arr =
  let n = Array.length arr in
  let dp = Array.make n 0 in
  dp.(0) <- arr.(0).(0);
  for i = 1 to n - 1 do
    let row = arr.(i) in
    dp.(i) <- row.(i) + dp.(i - 1);
    for j = i - 1 downto 1 do
      dp.(j) <- row.(j) + max dp.(j - 1) dp.(j)
    done;
    dp.(0) <- row.(0) + dp.(0)
  done;
  Array.fold_left Int.max 0 dp

let parse_input chan =
  In_channel.input_lines chan
  |> List.map (fun line ->
      String.split_on_char ' ' line |> Array.of_list |> Array.map int_of_string)
  |> Array.of_list

let () = parse_input In_channel.stdin |> max_path_sum |> Printf.printf "%d\n"
