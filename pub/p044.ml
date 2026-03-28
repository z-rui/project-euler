(** Pentagon Numbers *)

(* Sequence: 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
 * Difference:  4,  7, 10, 13, 16, 19, 22,  25,  28, ...
 * 2nd difference:  3,  3,  3,  3,  3,  3,   3,   3, ...
 *)

let is_pentagon x =
  let det = 1 + (24 * x) in
  let r = Float.(of_int det |> sqrt |> round |> to_int) in
  r * r = det && (r + 1) mod 6 = 0

let pentagon_mindiff () =
  let mindiff = ref Int.max_int in
  let rec loop_i x d =
    if d < !mindiff then begin
      let rec loop_j y d =
        let diff = x - y in
        if diff < !mindiff then begin
          if is_pentagon diff && is_pentagon (x + y) then mindiff := diff;
          if d > 1 then loop_j (y - d) (d - 3)
        end
      in
      loop_j (x - d) (d - 3);
      loop_i (x + d + 3) (d + 3)
    end
  in
  loop_i 5 4;
  !mindiff

let () =
  pentagon_mindiff () |> print_int;
  print_newline ()
