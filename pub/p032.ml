(* zarith is needed for trailing_zeros and popcount.
 * We can use int in Ocaml 5.5 where these functions are supported.
 *)

let digits_bitmap n =
  let rec aux acc i = function
    | 0 -> (Z.of_int acc, i)
    | x ->
        let b = 1 lsl (x mod 10) in
        aux (acc lor b) (i + 1) (x / 10)
  in
  aux 0 0 n

let iter_bit f z =
  let rec aux z =
    if not Z.(equal zero z) then begin
      let b = Z.(logand z (-z)) in
      f b;
      aux (Z.logxor z b)
    end
  in
  aux z

let pandigital_products digits =
  let products = ref [] in
  let next loop x unused =
    iter_bit
      begin fun b ->
        let x' = (x * 10) + Z.trailing_zeros b
        and unused' = Z.logxor unused b in
        loop x' unused'
      end
      unused
  in
  let rec loop_a a unused =
    let rec loop_b b unused =
      let prod = a * b in
      let dbm, n = digits_bitmap prod in
      if a < b && n = Z.popcount dbm && Z.equal dbm unused then
        products := prod :: !products;
      if n < Z.popcount unused then next loop_b b unused
    in
    loop_b 0 unused;
    next loop_a a unused
  in
  loop_a 0 digits;
  List.sort_uniq Int.compare !products

let () =
  pandigital_products Z.(~$0x3fe)
  |> List.fold_left ( + ) 0 |> Printf.printf "%d\n"
