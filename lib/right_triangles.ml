let primitives p_max =
  Seq.ints 2
  |> Seq.take_while (fun m -> 2 * m * (m + 1) <= p_max)
  |> Seq.concat_map begin fun m ->
      Seq.ints 1
      |> Seq.take_while (fun n -> n < m && 2 * m * (m + n) <= p_max)
      |> Seq.filter_map begin fun n ->
          (* Euclid's formula:
           *     a = m^2 - n^2, b = 2mn, c = m^2 + n^2
           * with m and n coprime and of opposite parities.
           *)
          if m mod 2 <> n mod 2 && Num.gcd m n = 1 then
            let mm = m * m and nn = n * n in
            let a = mm - nn and b = 2 * m * n and c = mm + nn in
            Some (a, b, c)
          else None
        end
    end

let count_by_perimeter p_max =
  let count = Array.make (p_max + 1) 0 in
  primitives p_max
  |> Seq.iter begin fun (a, b, c) ->
      let rec loop acc p =
        count.(acc) <- count.(acc) + 1;
        let acc' = acc + p in
        if acc' <= p_max then loop acc' p
      in
      let p = a + b + c in
      loop p p
    end;
  count
