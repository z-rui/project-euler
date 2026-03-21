(** Cubic Permutations *)

let count_digits n =
  let cnt = Array.make 10 0 in
  let rec aux n =
    if n > 0 then begin
      let d = n mod 10 in
      cnt.(d) <- cnt.(d) + 1;
      aux (n / 10)
    end
  in
  aux n;
  cnt

let rec search scale n =
  let scale' = scale * 10 in
  let counter = Hashtbl.create scale in
  for n = scale to scale' - 1 do
    let cube = n * n * n in
    let digits = count_digits cube in
    Hashtbl.replace counter digits
      begin match Hashtbl.find_opt counter digits with
      | Some l -> cube :: l
      | None -> [ cube ]
      end
  done;
  let find_solution _ l acc =
    if List.length l = n then
      let min_elt = List.fold_left Int.min (List.hd l) (List.tl l) in
      match acc with
      | None -> Some min_elt
      | Some x when x > min_elt -> Some min_elt
      | _ -> acc
    else acc
  in
  match Hashtbl.fold find_solution counter None with
  | None -> search (scale * 10) n
  | Some x -> x

let () =
  search 2 5 |> print_int;
  print_newline ()
