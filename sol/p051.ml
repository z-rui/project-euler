(** Prime Digit Replacements *)

let prime_table = Euler.Prime_table.create 1_100_000

let mask_off n posmask f =
  let rec aux acc seen i = function
    | 0 -> f acc
    | n ->
        let d = n mod 10 in
        let shift = 4 * i in
        let i' = i + 1 and n' = n / 10 in
        if (1 lsl i) land posmask <> 0 then begin
          let seen' = 1 lsl d in
          if seen' lor seen = seen' then
            aux ((0xf lsl shift) lor acc) seen' i' n'
        end
        else aux ((d lsl shift) lor acc) seen i' n'
  in
  aux 0 0 0 n

let rec search prime_idx w scale family_size =
  let module H = Hashtbl.Make (Int) in
  let scale' = scale * 10 in
  let patterns = H.create (1 lsl (3 * w)) in
  let allbits = (1 lsl w) - 1 in
  let rec loop prime_idx =
    if prime_idx >= Array.length prime_table.primes then
      failwith "run out of primes; extend prime table";
    let p = prime_table.primes.(prime_idx) in
    if p >= scale' then prime_idx
    else begin
      for bitmask = 1 to allbits - 1 do
        mask_off p bitmask @@ fun s' ->
        match H.find_opt patterns s' with
        | Some (n, _) -> incr n
        | None -> H.add patterns s' (ref 1, p)
      done;
      loop (prime_idx + 1)
    end
  in
  let prime_idx' = loop prime_idx in

  (* All patterns of width w have been visited. *)
  match
    H.fold
      begin fun _ (cnt, p) acc ->
        if !cnt = family_size then
          match acc with
          | None -> Some p
          | Some p' when p' > p -> Some p
          | _ -> acc
        else acc
      end
      patterns None
  with
  | Some p -> p
  | None -> search prime_idx' (w + 1) scale' family_size

let () =
  (* 2 3 5 7 [11], first idx = 4 *)
  search 4 2 10 8 |> print_int;
  print_newline ()
