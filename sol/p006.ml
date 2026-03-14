let nums = Seq.ints 1 |> Seq.take 100
let sum_of_squares = Seq.map2 ( * ) nums nums |> Seq.fold_left ( + ) 0

let square_of_sum =
  let sqr = Seq.fold_left ( + ) 0 nums in
  sqr * sqr

let () = Printf.printf "%d\n" (square_of_sum - sum_of_squares)
