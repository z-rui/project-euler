(** Magic 5-gon Ring *)

(* Dumb brute force, many unnecessary enumerations, but works for the given
   problem size. *)
let solve n expected_length =
  let graph =
    Array.init n @@ fun i ->
    let j = if i + 1 = n then 0 else i + 1 in
    (n + i, i, j)
  in
  let sol_to_string a =
    let b = Array.map (fun (i, j, k) -> (a.(i), a.(j), a.(k))) graph in
    let min_idx = ref 0 in
    let n = Array.length b in
    for i = 1 to n - 1 do
      if b.(i) < b.(!min_idx) then min_idx := i
    done;
    let buf = Buffer.create (expected_length + 1) in
    for i = 0 to n - 1 do
      let x, y, z = b.((i + !min_idx) mod n) in
      Printf.bprintf buf "%d%d%d" x y z
    done;
    Buffer.contents buf
  in
  let check a =
    let sum idx =
      let i, j, k = graph.(idx) in
      a.(i) + a.(j) + a.(k)
    in
    let rec aux idx x = idx = 0 || (sum idx = x && aux (idx - 1) x) in
    aux (Array.length graph - 1) (sum 0)
  in
  let max_sol = ref None in
  let rec loop a =
    if check a then begin
      let sol = sol_to_string a in
      if String.length sol = expected_length then
        match !max_sol with
        | None -> max_sol := Some sol
        | Some sol' when sol' < sol -> max_sol := Some sol
        | _ -> ()
    end;
    if Euler.next_permutation Int.compare a then loop a
  in
  loop (Array.init (2 * n) (fun i -> i + 1));
  !max_sol

let () =
  match solve 5 16 with
  | Some sol -> print_endline sol
  | None -> print_endline "no solution found."
