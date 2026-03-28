(** Prime Digit Replacements *)

let prime_table = Euler.Prime_table.create 1_100_000

let iter_patterns n f =
  let rec aux acc used i = function
    | 0 -> if used <> 0 then f acc
    | n ->
        let d = n mod 10 in
        let shift = 4 * i in
        let i' = i + 1 and n' = n / 10 in
        let this = 1 lsl d in
        if this lor used = this then aux ((0xf lsl shift) lor acc) this i' n';
        aux ((d lsl shift) lor acc) used i' n'
  in
  aux 0 0 0 n

let rec search prime_idx scale family_size =
  let module H = struct
    include Hashtbl.Make (Int)

    type elt = { mutable cnt : int; p : int }
  end in
  let scale' = scale * 10 in
  let patterns : H.elt H.t = H.create (scale * 2) in
  let rec loop prime_idx =
    if prime_idx >= Array.length prime_table.primes then
      failwith "run out of primes; extend prime table";
    let p = prime_table.primes.(prime_idx) in
    if p >= scale' then prime_idx
    else begin
      iter_patterns p begin fun pat ->
          match H.find_opt patterns pat with
          | Some ent -> ent.cnt <- ent.cnt + 1
          | None -> H.add patterns pat { cnt = 1; p }
        end;
      loop (prime_idx + 1)
    end
  in
  let prime_idx' = loop prime_idx in

  (* All patterns of this width have been visited. *)
  let foldfunc _ ({ cnt; p } : H.elt) acc =
    if cnt = family_size then
      match acc with None -> Some p | Some p' when p' > p -> Some p | _ -> acc
    else acc
  in
  match H.fold foldfunc patterns None with
  | Some p -> p
  | None -> search prime_idx' scale' family_size

let () =
  (* 2 3 5 7 [11], first idx = 4 *)
  search 4 10 8 |> print_int;
  print_newline ()
