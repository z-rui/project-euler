(** Passcode Derivation *)

let n = 10

let parse_input chan =
  let exists = ref 0 and before = Array.make n 0 and deg = Array.make n 0 in
  let set_less x y =
    if before.(x) land (1 lsl y) = 0 then begin
      before.(x) <- before.(x) lor (1 lsl y);
      deg.(y) <- deg.(y) + 1
    end
  in
  In_channel.input_lines chan
  |> List.iter begin fun line ->
      let nodes = Array.init 3 @@ fun i -> Char.code line.[i] - Char.code '0' in
      Array.iter (fun x -> exists := !exists lor (1 lsl x)) nodes;
      set_less nodes.(0) nodes.(1);
      set_less nodes.(1) nodes.(2)
    end;
  (!exists, before, deg)

let () =
  let exists, before, deg = parse_input stdin in
  let remaining = ref exists in
  while !remaining <> 0 do
    for x = 0 to n - 1 do
      if !remaining land (1 lsl x) <> 0 && deg.(x) = 0 then begin
        print_int x;
        remaining := !remaining lxor (1 lsl x);
        for y = 0 to n - 1 do
          if before.(x) land (1 lsl y) <> 0 then deg.(y) <- deg.(y) - 1
        done
      end
    done
  done;
  print_newline ()
