let digit_rotations n =
  let scale =
    let rec aux acc =
      let acc' = 10 * acc in
      if acc' > n then acc else aux acc'
    in
    aux 1
  in
  let rec next x =
    Seq.Cons
      ( x,
        fun () ->
          let x' = (x / 10) + (x mod 10 * scale) in
          if x' = n then Seq.Nil else next x' )
  in
  fun () -> next n

let () =
  let n = 1_000_000 in
  let primetab = Euler.Prime_table.create n in
  Seq.ints 2
  |> Seq.take (n - 2)
  |> Seq.filter (fun x ->
      digit_rotations x |> Seq.for_all (fun x -> primetab.is_prime.(x)))
  |> Seq.length |> Printf.printf "%d\n"
