(** Prime Digit Replacements *)

module H = Hashtbl.Make (String)

let prime_table = Euler.Prime_table.create 1_100_000
let max_prime = prime_table.primes.(Array.length prime_table.primes - 1)

let next_prime =
  let arr = Array.make max_prime 0 in
  let j = ref 0 in
  let p = ref 2 in
  for i = 0 to max_prime - 1 do
    if i >= !p then begin
      incr j;
      p := prime_table.primes.(!j)
    end;
    arr.(i) <- !p
  done;
  arr

let[@inline] char_to_digit c = Char.code c - Char.code '0'

let all_same_char s posmask =
  let n = String.length s in
  let rec loop seen i =
    if i = n then true
    else
      let b = 1 lsl i in
      if b land posmask <> 0 then
        let digit_mask = 1 lsl char_to_digit s.[i] in
        digit_mask lor seen = digit_mask && loop digit_mask (i + 1)
      else loop seen (i + 1)
  in
  loop 0 0

let rec search w scale family_size =
  let scale' = scale * 10 in
  let patterns = H.create (1 lsl (3 * w)) in
  let allbits = (1 lsl w) - 1 in
  let rec loop p =
    let s = Int.to_string p in
    for bitmask = 1 to allbits - 1 do
      if all_same_char s bitmask then begin
        let s' =
          String.init w @@ fun i ->
          if (1 lsl i) land bitmask <> 0 then '*' else s.[i]
        in
        match H.find_opt patterns s' with
        | Some (n, _) -> incr n
        | None -> H.add patterns s' (ref 1, p)
      end
    done;
    if p = max_prime then failwith "run out of primes; extend prime table";
    let p' = next_prime.(p) in
    if p' < scale' then loop p'
  in
  loop next_prime.(scale);

  (* All patterns of width w have been visited. *)
  match
    H.fold
      begin fun s (cnt, p) acc ->
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
  | None -> search (w + 1) scale' family_size

let () =
  search 2 10 8 |> print_int;
  print_newline ()
