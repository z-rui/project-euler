let next_permutation cmp a =
  let n = Array.length a in
  let swap i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  in
  let rec rev i j =
    if i < j then begin
      swap i j;
      rev (i + 1) (j - 1)
    end
  in
  let rec loop i =
    if i < 0 then false
    else if cmp a.(i + 1) a.(i) <= 0 then loop (i - 1)
    else begin
      let j =
        let rec loop' acc j =
          if j = n then acc
          else if cmp a.(i) a.(j) < 0 && cmp a.(j) a.(acc) < 0 then
            loop' j (j + 1)
          else loop' acc (j + 1)
        in
        loop' (i + 1) (i + 2)
      in
      swap i j;
      rev (i + 1) (n - 1);
      true
    end
  in
  loop (n - 2)
