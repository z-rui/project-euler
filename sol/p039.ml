let integer_right_triangles p_max =
  let result = Array.make (p_max + 1) [] in
  Seq.ints 2
  |> Seq.take_while (fun x -> x * x <= p_max)
  |> Seq.iter begin fun m ->
      for n = 1 to m - 1 do
        (* Euclid's formula:
         *     a = m^2 - n^2, b = 2mn, c = m^2 + n^2
         * with m and n coprime and of opposite parities.
         *)
        if m mod 2 <> n mod 2 && Euler.gcd m n = 1 then
          let a = (m * m) - (n * n)
          and b = 2 * m * n
          and c = (m * m) + (n * n) in
          let rec loop a' b' c' =
            let p = a' + b' + c' in
            if p <= p_max then begin
              result.(p) <- (a', b', c') :: result.(p);
              loop (a' + a) (b' + b) (c' + c)
            end
          in
          loop a b c
      done
    end;
  result

let () =
  integer_right_triangles 1000
  |> Array.to_seq
  |> Seq.mapi (fun i x -> (List.length x, i))
  |> Seq.fold_left max (0, -1)
  |> fun (cnt, i) -> Printf.printf "i=%d cnt=%d\n" i cnt
