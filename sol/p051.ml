(** Prime Digit Replacements *)

(* Note: takes 0.5s on my machine.
 * Slower than earlier problems, but acceptable.
 * Took the shortcut to use Z.nextprime instead of precomputing
 * the prime table (for no upper bound is known).
 * Profiling shows most time spent on Hashtbl and all_same_char,
 * so I'm fine with using Z.
 *)

module H = Hashtbl.Make (String)

let[@inline] char_to_digit c = Char.code c - Char.code '0'
let[@inline] digit_to_char d = Char.chr (Char.code '0' + d)

let all_same_char s bitmask =
  let n = String.length s in
  let rec loop mask i =
    if i = n then true
    else
      let b = 1 lsl i in
      if b land bitmask <> 0 then
        let digit_mask = 1 lsl char_to_digit s.[i] in
        digit_mask lor mask = digit_mask && loop digit_mask (i + 1)
      else loop mask (i + 1)
  in
  loop 0 0

let search w family_size =
  let patterns = H.create (1 lsl (3 * w)) in
  let allbits = (1 lsl w) - 1 in
  let rec loop p =
    let s = Z.to_string p in
    if String.length s = w then begin
      for bitmask = 1 to allbits - 1 do
        if all_same_char s bitmask then
          let s' =
            String.init w (fun i ->
                if (1 lsl i) land bitmask <> 0 then '*' else s.[i])
          in
          H.replace patterns s'
            (match H.find_opt patterns s' with
            | Some (n, p') -> (n + 1, Z.min p p')
            | None -> (1, p))
      done;
      loop (Z.nextprime p)
    end
  in
  loop (Z.nextprime (Z.pow (Z.of_int 10) (w - 1)));

  (* All patterns have been visited, now see if there is a solution. *)
  let found = ref false in
  H.iter
    begin fun s (cnt, p) ->
      if cnt = family_size then begin
        Printf.printf "%s -> count=%d, min_prime=%s\n" s cnt (Z.to_string p);
        found := true
      end
    end
    patterns;
  !found

let () =
  let rec loop w = if not (search w 8) then loop (w + 1) in
  loop 2
