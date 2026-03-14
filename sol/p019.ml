(** Counting Sundays *)

let is_leap_year y = y mod 400 = 0 || (y mod 4 = 0 && y mod 100 <> 0)

let month_days leap = function
  | 1 -> 31
  | 2 -> if leap then 29 else 28
  | 3 -> 31
  | 4 -> 30
  | 5 -> 31
  | 6 -> 30
  | 7 -> 31
  | 8 -> 31
  | 9 -> 30
  | 10 -> 31
  | 11 -> 30
  | 12 -> 31
  | _ -> invalid_arg "month must be 1..12"

let solve first_year last_year first_day =
  let sunday_count = ref 0 in
  let day = ref first_day in
  for year = first_year to last_year do
    let leap = is_leap_year year in
    for month = 1 to 12 do
      if !day = 0 then incr sunday_count;
      day := (!day + month_days leap month) mod 7
    done
  done;
  !sunday_count

let () = solve 1901 2000 2 |> Printf.printf "%d\n"
