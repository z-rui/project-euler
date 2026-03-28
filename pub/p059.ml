let crypt ~out cipher key =
  let key_len = Array.length key in
  Array.iteri
    (fun i x -> Bytes.set out i @@ Char.chr (x lxor key.(i mod key_len)))
    cipher

let strstr haystack needle =
  let rec loop i j =
    if j = String.length needle then Some i
    else if i > String.length haystack - String.length needle then None
    else if haystack.[i + j] = needle.[j] then loop i (j + 1)
    else loop (i + 1) 0
  in
  loop 0 0

let () =
  let codepoints =
    In_channel.(input_line stdin)
    |> Option.get |> String.split_on_char ',' |> Array.of_list
    |> Array.map int_of_string
  in
  let key = Array.make 3 0 in
  let buf = Bytes.create (Array.length codepoints) in
  for a = Char.code 'a' to Char.code 'z' do
    key.(0) <- a;
    for b = Char.code 'a' to Char.code 'z' do
      key.(1) <- b;
      for c = Char.code 'a' to Char.code 'z' do
        key.(2) <- c;
        crypt ~out:buf codepoints key;
        let s = Bytes.unsafe_to_string buf in
        if Option.is_some @@ strstr s " the " then begin
          Printf.printf "%s\n\nkey = %c%c%c\n" s (Char.chr a) (Char.chr b)
            (Char.chr c);
          String.fold_left (fun acc c -> acc + Char.code c) 0 s
          |> Printf.printf "checksum = %d\n"
        end
      done
    done
  done
