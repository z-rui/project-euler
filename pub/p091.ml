(** Right Triangle with Integer Coordinates *)

let dotp x1 y1 x2 y2 = (x1 * x2) + (y1 * y2)

let right_triangle x1 y1 x2 y2 =
  let dx = x2 - x1 and dy = y2 - y1 and d = (x1 * y2) - (x2 * y1) in
  d <> 0
  && (dotp x1 y1 x2 y2 = 0 || dotp x1 y1 dx dy = 0 || dotp x2 y2 dx dy = 0)

let () =
  let n = 50 in
  let cnt = ref 0 in
  for x1 = 0 to n do
    let y1_start = if x1 = 0 then 1 else 0 in
    for y1 = y1_start to n do
      for x2 = x1 to n do
        let y2_start = if x1 = x2 then y1 + 1 else 0 in
        for y2 = y2_start to n do
          if right_triangle x1 y1 x2 y2 then incr cnt
        done
      done
    done
  done;
  print_int !cnt;
  print_newline ()
