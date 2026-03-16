let primetab = Euler.Prime_table.create 10_000
let combine = Seq.fold_left (fun acc d -> (acc * 10) + d) 0

let explode =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux ((n mod 10) :: acc) (n / 10)
  in
  aux []

let hash = List.sort Int.compare

let () =
  Array.to_seq primetab.primes
  |> Seq.drop_while (fun x -> x < 1000)
  |> Seq.iter @@ fun p ->
     let digits = explode p in
     let digits' = Array.of_list digits in
     let h = hash digits in
     while Euler.next_permutation Int.compare digits' do
       let q = combine (Array.to_seq digits') in
       assert (q > p);
       if primetab.is_prime.(q) then
         let r = q + q - p in
         if r <= 9999 && primetab.is_prime.(r) && hash (explode r) = h then
           Printf.printf "%d%d%d\n" p q r
     done
