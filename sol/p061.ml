(** Cyclical Figurate Numbers *)

let solve n =
  let p = Array.make n 1 and d = Array.init n (fun i -> i + 2) in
  let numtype = Array.make 10000 0 and chain = Array.make 100 Z.zero in
  while p.(0) <= 9999 do
    for k = 0 to n - 1 do
      let num = p.(k) in
      if num >= 1000 && num <= 9999 then begin
        numtype.(num) <- numtype.(num) lor (1 lsl k);
        let hi = num / 100 and lo = num mod 100 in
        chain.(hi) <- Z.(chain.(hi) lor (one lsl lo))
      end;
      let diff = d.(k) in
      p.(k) <- num + diff;
      d.(k) <- diff + k + 1
    done
  done;
  let rec find_loop allowed acc hi start =
    if allowed = 0 then begin
      let acc = List.rev acc and acc' = List.sort_uniq Int.compare acc in
      if hi = start && List.(length acc' = n && hd acc = hd acc') then begin
        print_string "Solution:";
        List.iter (fun x -> Printf.printf " %d(%x)" x numtype.(x)) acc;
        Printf.printf "\nSum: %d\n" (List.fold_left ( + ) 0 acc)
      end
    end
    else
      let nexts = ref chain.(hi) in
      while not Z.(equal zero !nexts) do
        let b = Z.(!nexts land - !nexts) in
        let next = Z.trailing_zeros b in
        let num = (hi * 100) + next in
        let types = ref numtype.(num) in
        while !types <> 0 do
          let b' = !types land - !types in
          if allowed land b' <> 0 then
            find_loop (allowed lxor b') (num :: acc) next start;
          types := !types lxor b'
        done;
        nexts := Z.(!nexts lxor b)
      done
  in
  for start = 10 to 99 do
    find_loop ((1 lsl n) - 1) [] start start
  done

let () = solve 6
