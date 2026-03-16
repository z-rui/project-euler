let consecutive_search k =
  let cache = Dynarray.make 2 0 in
  let prime_factor_count n =
    let p =
      if n mod 2 = 0 then 2
      else
        let rec aux i =
          if i * i > n then n else if n mod i = 0 then i else aux (i + 2)
        in
        aux 3
    in
    let n' =
      let rec aux n' = if n' mod p = 0 then aux (n' / p) else n' in
      aux (n / p)
    in
    let cnt = Dynarray.get cache n' + 1 in
    assert (Dynarray.length cache = n);
    Dynarray.add_last cache cnt;
    cnt
  in
  let rec loop count n =
    if count = k then n - k
    else if k = prime_factor_count n then loop (count + 1) (n + 1)
    else loop 0 (n + 1)
  in
  loop 0 2

let () =
  consecutive_search 4 |> print_int;
  print_newline ()
