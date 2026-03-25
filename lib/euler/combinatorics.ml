let next_permutation_sub cmp a first last =
  let[@inline] swap i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  in
  let[@inline] rec rev i j =
    if i < j then begin
      swap i j;
      rev (i + 1) (j - 1)
    end
  in
  let[@inline] rec loop i =
    if i < first then false
    else if cmp a.(i + 1) a.(i) <= 0 then loop (i - 1)
    else begin
      let[@inline] rec loop' acc j =
        if j > last then acc
        else if cmp a.(i) a.(j) < 0 && cmp a.(j) a.(acc) < 0 then loop' j (j + 1)
        else loop' acc (j + 1)
      in
      let j = loop' (i + 1) (i + 2) in
      swap i j;
      rev (i + 1) last;
      true
    end
  in
  loop (last - 1)

let next_permutation cmp a = next_permutation_sub cmp a 0 (Array.length a - 1)

let bit_combinations n k f =
  let rec aux len curr min =
    if len = k then f curr
    else
      for i = min to n - 1 do
        aux (len + 1) (curr lor (1 lsl i)) (i + 1)
      done
  in
  aux 0 0 0
