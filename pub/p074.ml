(** Digit Factorial Chains *)

let fac_table =
  let fac = Array.make 10 1 in
  for i = 1 to 9 do
    fac.(i) <- i * fac.(i - 1)
  done;
  fac

let digit_factorial_sum n =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux (acc + fac_table.(n mod 10)) (n / 10)
  in
  aux 0 n

let non_repeating_length_count max target =
  let cache_size = digit_factorial_sum max + 1 in
  let cache = Array.make cache_size 0 in
  let rec count curr =
    match cache.(curr) with
    | 0 ->
        let rec mark steps curr =
          let cnt = cache.(curr) in
          if cnt > 0 then (steps, cnt)
          else if cnt < 0 then
            let loop_start = lnot cnt in
            let loop_len = steps - loop_start in
            (loop_start, loop_len)
          else begin
            (* negative number to indicate its position of the call chain *)
            cache.(curr) <- lnot steps;
            let ((loop_start, len) as result) =
              mark (steps + 1) (digit_factorial_sum curr)
            in
            (* overwrite with the actual value *)
            cache.(curr) <- Int.max 0 (loop_start - steps) + len;
            result
          end
        in
        let loop_start, len = mark 0 curr in
        loop_start + len
    | cnt -> cnt
  in
  let cnt = ref 0 in
  for n = 1 to max do
    if count n = target then incr cnt
  done;
  !cnt

let () =
  non_repeating_length_count 999_999 60 |> print_int;
  print_newline ()
