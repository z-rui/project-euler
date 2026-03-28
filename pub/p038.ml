(** Pandigital Multiples *)

let check_pandigital l =
  let count = Array.make 10 0 in
  count.(0) <- 1;
  let rec update x =
    if x > 0 then begin
      let d = x mod 10 in
      count.(d) <- count.(d) + 1;
      update (x / 10)
    end
  in
  List.iter update l;
  if Array.for_all (Int.equal 1) count then `Found
  else if Array.exists (fun x -> x > 1) count then `Prune
  else `Recur

let pandigital_multiples generator =
  let result = ref [] in
  let rec aux acc i =
    let acc' = (generator * i) :: acc in
    match check_pandigital acc' with
    | `Found ->
        let s = List.rev_map Int.to_string acc' |> String.concat "" in
        result := s :: !result
    | `Recur -> aux acc' (i + 1)
    | `Prune -> ()
  in
  aux [] 1;
  !result

let () =
  Seq.ints 1 |> Seq.take 9999
  |> Seq.concat_map (fun n -> pandigital_multiples n |> List.to_seq)
  |> Seq.fold_left max "" |> print_endline
