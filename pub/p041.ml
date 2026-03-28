let pandigital_primes n =
  let rec aux x unused =
    if Z.(equal zero unused) then
      match Z.probab_prime x 5 with
      | 0 -> ()
      | 2 -> print_endline (Z.to_string x)
      | _ -> failwith "prime test inconclusive"
    else
      let z = ref unused in
      while not Z.(equal zero !z) do
        let b = Z.(!z land - !z) in
        (z := Z.(!z lxor b));
        aux Z.((x * ~$10) + ~$(trailing_zeros b)) Z.(unused lxor b)
      done
  in
  let n' = n + 1 in
  aux Z.zero Z.((one lsl n') - ~$2)

let () =
  for n = 2 to 9 do
    pandigital_primes n
  done
