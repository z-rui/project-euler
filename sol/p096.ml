(* Su Doku *)

let print_solution state =
  for i = 0 to 8 do
    for j = 0 to 8 do
      Printf.printf "%2d" state.(i).(j)
    done;
    print_char '\n'
  done

let solve state =
  let row_ok = Array.make 9 0b11_1111_1110
  and col_ok = Array.make 9 0b11_1111_1110
  and sqr_ok = Array.make 9 0b11_1111_1110 in
  for i = 0 to 8 do
    for j = 0 to 8 do
      let d = state.(i).(j) in
      let bm = lnot (1 lsl d) in
      let k = (i / 3 * 3) + (j / 3) in
      row_ok.(i) <- row_ok.(i) land bm;
      col_ok.(j) <- col_ok.(j) land bm;
      sqr_ok.(k) <- sqr_ok.(k) land bm
    done
  done;
  (* straightforward brute-force *)
  let rec search i j =
    if i = 9 then raise Exit;
    if state.(i).(j) <> 0 then search_next i j
    else
      let k = (i / 3 * 3) + (j / 3) in
      let allowed' = row_ok.(i) land col_ok.(j) land sqr_ok.(k) in
      for num = 1 to 9 do
        let num_bm = 1 lsl num in
        if allowed' land num_bm <> 0 then begin
          state.(i).(j) <- num;
          row_ok.(i) <- row_ok.(i) lxor num_bm;
          col_ok.(j) <- col_ok.(j) lxor num_bm;
          sqr_ok.(k) <- sqr_ok.(k) lxor num_bm;
          search_next i j;
          row_ok.(i) <- row_ok.(i) lxor num_bm;
          col_ok.(j) <- col_ok.(j) lxor num_bm;
          sqr_ok.(k) <- sqr_ok.(k) lxor num_bm;
          state.(i).(j) <- 0
        end
      done
  and search_next i j = if j = 8 then search (i + 1) 0 else search i (j + 1) in
  try search 0 0 with Exit -> print_solution state

let parse_input chan =
  match In_channel.input_line chan with
  | Some line ->
      Option.some
      @@ Array.init 9 begin fun _ ->
          match In_channel.input_line chan with
          | Some line ->
              Array.init 9 (fun j -> Char.code line.[j] - Char.code '0')
          | None -> failwith "unexpected EOF"
        end
  | None -> None

let () =
  let sum = ref 0 in
  let rec loop i =
    match parse_input stdin with
    | Some state ->
        Printf.printf "Grid %02d\n%!" i;
        solve state;
        let row = state.(0) in
        sum := !sum + (100 * row.(0)) + (10 * row.(1)) + row.(2);
        loop (i + 1)
    | None -> ()
  in
  loop 1;
  Printf.printf "%d\n" !sum
