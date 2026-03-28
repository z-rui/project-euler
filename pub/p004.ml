let reverse n =
  let rec loop acc = function
    | 0 -> acc
    | m -> loop ((acc * 10) + (m mod 10)) (m / 10)
  in
  loop 0 n

let is_palindrome n = n = reverse n

let () =
  let largest_parlindrome = ref 0 in
  for x = 999 downto 100 do
    for y = x downto 100 do
      let p = x * y in
      if p > !largest_parlindrome && is_palindrome p then
        largest_parlindrome := p
    done
  done;
  Printf.printf "%d\n" !largest_parlindrome
