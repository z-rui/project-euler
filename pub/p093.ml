(** Arithmetic Expressions *)

let arith_expr_span digits =
  let reachable = Array.make 10000 false in
  let rec search stack digits =
    begin match stack with
    | [ x ] when digits = 0 ->
        if Q.den x = Z.one then
          let num = Z.to_int (Q.num x) in
          if num >= 0 then reachable.(num) <- true
    | x :: y :: rest ->
        search (Q.(y + x) :: rest) digits;
        search (Q.(y - x) :: rest) digits;
        search (Q.(y * x) :: rest) digits;
        if not Q.(equal zero x) then search (Q.(y / x) :: rest) digits
    | _ -> ()
    end;
    for d = 0 to 9 do
      let b = 1 lsl d in
      if digits land b <> 0 then search (Q.of_int d :: stack) (digits lxor b)
    done
  in
  search [] digits;
  let rec find i = if not reachable.(i + 1) then i else find (i + 1) in
  find 0

let () =
  let max_span = ref 0 in
  let max_span_digits = ref 0 in
  Euler.Combinatorics.bit_combinations 10 4 begin fun digits ->
      let span = arith_expr_span digits in
      if span > !max_span then begin
        max_span := span;
        max_span_digits := digits
      end
    end;
  for i = 0 to 9 do
    if !max_span_digits land (1 lsl i) <> 0 then print_int i
  done;
  print_newline ()
