(** Path Sum: Four Ways *)

let min_path_sum matrix rows cols =
  (* Dijkstra's shortest path algorithm *)
  let module Search_record = struct
    type t = { id : int; dist : int }

    let compare { dist = d1 } { dist = d2 } = Int.compare d1 d2
  end in
  let module PQ = Pqueue.MakeMin (Search_record) in
  let min_dist = Array.make (rows * cols) Int.max_int in
  let q = PQ.create () in
  min_dist.(0) <- matrix.(0).(0);
  let rec loop id dist =
    if dist = min_dist.(id) then begin
      let update i j =
        let id = (i * cols) + j in
        let dist' = dist + matrix.(i).(j) in
        if dist' < min_dist.(id) then begin
          min_dist.(id) <- dist';
          PQ.add q { id; dist = dist' }
        end
      in
      let i = id / cols and j = id mod cols in
      (* propagate at 4 directions *)
      if i > 0 then update (i - 1) j;
      if j > 0 then update i (j - 1);
      if j < cols - 1 then update i (j + 1);
      if i < rows - 1 then update (i + 1) j
    end;
    match PQ.pop_min q with Some { id; dist } -> loop id dist | None -> ()
  in
  loop 0 min_dist.(0);
  min_dist.((rows * cols) - 1)

let () =
  let matrix = Euler.Matrix_file.parse int_of_string In_channel.stdin in
  min_path_sum matrix (Array.length matrix) (Array.length matrix.(0))
  |> print_int;
  print_newline ()
