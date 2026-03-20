type suit = Spade | Heart | Club | Diamond
type card = int * suit

let parse_card s : card =
  if String.length s <> 2 then invalid_arg "parse_card";
  let num =
    match s.[0] with
    | '2' .. '9' as c -> Char.code c - Char.code '2'
    | 'T' -> 8
    | 'J' -> 9
    | 'Q' -> 10
    | 'K' -> 11
    | 'A' -> 12
    | _ -> invalid_arg "parse_card"
  and suit =
    match s.[1] with
    | 'S' -> Spade
    | 'H' -> Heart
    | 'C' -> Club
    | 'D' -> Diamond
    | _ -> invalid_arg "parse_card"
  in
  (num, suit)

type hand =
  (* The int list at the end are the "high cards" for tie breaking *)
  | High_card of int list (* descending order *)
  | One_pair of int * int list
  | Two_pairs of int * int (* descending order *) * int list
  | Three_of_a_kind of int * int list
  | Straight of int (* largest number *)
  | Flush of int list (* descending order *)
  | Full_house of int * int (* three followed by pair *)
  | Four_of_a_kind of int * int list
  | Straight_flush of int (* largest number *)

let compute_hand cards : hand =
  if List.length cards <> 5 then invalid_arg "a hand must be exactly 5 cards";
  let count_num = Array.make 13 0 and count_suit = Array.make 4 0 in
  let do_count (num, suit) =
    count_num.(num) <- count_num.(num) + 1;
    let s =
      match suit with Spade -> 0 | Heart -> 1 | Club -> 2 | Diamond -> 3
    in
    count_suit.(s) <- count_suit.(s) + 1
  in
  List.iter do_count cards;
  let buckets = Array.make 4 [] in
  Array.iteri
    (fun n cnt -> if cnt > 0 then buckets.(cnt - 1) <- n :: buckets.(cnt - 1))
    count_num;
  match buckets with
  | [| ones; _; _; [ four ] |] -> Four_of_a_kind (four, ones)
  | [| _; [ two ]; [ three ]; _ |] -> Full_house (three, two)
  | [| ones; _; [ three ]; _ |] -> Three_of_a_kind (three, ones)
  | [| ones; [ high; low ]; _; _ |] -> Two_pairs (high, low, ones)
  | [| ones; [ two ]; _; _ |] -> One_pair (two, ones)
  | [| ones; []; []; [] |] -> begin
      let is_flush = Array.mem 5 count_suit
      and is_straight =
        match ones with
        | [ high; _; _; _; low ] when high - low = 4 -> Some high
        | [ 12; 3; 2; 1; 0 ] -> Some 3 (* special case for A 2 3 4 5 *)
        | _ -> None
      in
      match is_straight with
      | Some high -> if is_flush then Straight_flush high else Straight high
      | None -> if is_flush then Flush ones else High_card ones
    end
  | _ -> failwith "unrecognized pattern"

let parse_line s =
  match String.split_on_char ' ' s |> List.map parse_card with
  | c0 :: c1 :: c2 :: c3 :: c4 :: ([ _; _; _; _; _ ] as rhs) ->
      (compute_hand [ c0; c1; c2; c3; c4 ], compute_hand rhs)
  | _ -> failwith "parse_line: invalid format"

let () =
  In_channel.(input_lines stdin)
  |> List.fold_left
       begin fun acc line ->
         let lhs, rhs = parse_line line in
         (* OCaml's polymorphic comparison works well here *)
         match compare lhs rhs with
         | x when x > 0 -> acc + 1
         | x when x < 0 -> acc
         | _ -> failwith "tie (shouldn't happen in this problem)"
       end
       0
  |> print_int;
  print_newline ()
