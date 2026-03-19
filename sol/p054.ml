module Poker = struct
  module Suit = struct
    type t = Spade | Heart | Club | Diamond

    let of_char = function
      | 'S' -> Spade
      | 'H' -> Heart
      | 'C' -> Club
      | 'D' -> Diamond
      | _ -> failwith "invalid suit charactor"

    let to_int = function Spade -> 0 | Heart -> 1 | Club -> 2 | Diamond -> 3
  end

  let all_chars = "23456789TJQKA"
  let char_to_num c = String.index all_chars c
  let num_to_char i = all_chars.[i]

  module Card = struct
    type t = int * Suit.t

    let num ((n, _) : t) = n
    let compare (x, _) (y, _) = Int.compare x y

    let of_string s : t =
      if String.length s <> 2 then
        invalid_arg "Card.of_string: must have length of 2";
      (char_to_num s.[0], Suit.of_char s.[1])
  end

  module Counter = struct
    type t = int array * int array

    let create () : t = (Array.make 13 0, Array.make 4 0)

    let add ((count_num, count_suit) : t) ((num, suit) : Card.t) =
      count_num.(num) <- count_num.(num) + 1;
      let s = Suit.to_int suit in
      count_suit.(s) <- count_suit.(s) + 1

    let get_char ((count_num, _) : t) c = count_num.(char_to_num c)
    let get_suit ((_, count_suit) : t) suit = count_suit.(Suit.to_int suit)
  end

  module Hand = struct
    type t =
      (* The int list at the end are the "high cards" used for breaking a tie *)
      | High_card of int list (* descending order *)
      | One_pair of int * int list
      | Two_pairs of int * int (* descending order *) * int list
      | Three_of_a_kind of int * int list
      | Straight of int (* largest number *)
      | Flush of int list (* descending order *)
      | Full_house of int * int (* three followed by pair *)
      | Four_of_a_kind of int * int list
      | Straight_flush of int (* largest number *)

    let of_cards cards =
      if List.length cards <> 5 then
        invalid_arg "a hand must be exactly 5 cards";
      let ((count_num, count_suit) as count) = Counter.create () in
      List.iter (Counter.add count) cards;
      let count_rep = Array.make 4 [] in
      Array.iteri
        (fun n cnt ->
          if cnt > 0 then count_rep.(cnt - 1) <- n :: count_rep.(cnt - 1))
        count_num;
      let is_flush = Array.mem 5 count_suit in
      let is_straight =
        let rec aux cnt i n =
          if cnt = n then Some (i - 1)
          else if i >= Array.length count_num then None
          else
            let cnt' = match count_num.(i) with 1 -> cnt + 1 | _ -> 0 in
            aux cnt' (i + 1) n
        in
        match aux 0 0 5 with
        | Some _ as x -> x
        | None ->
            (* special case for A 2 3 4 5 *)
            begin match count_rep.(0) with
            | [ 12; 3; 2; 1; 0 ] -> Some 3
            | _ -> None
            end
      in
      if is_flush then
        match is_straight with
        | Some high -> Straight_flush high
        | _ -> Flush count_rep.(0)
      else
        match count_rep with
        | [| ones; _; _; [ four ] |] -> Four_of_a_kind (four, ones)
        | [| _; [ two ]; [ three ]; _ |] -> Full_house (three, two)
        | [| ones; _; [ three ]; _ |] -> Three_of_a_kind (three, ones)
        | [| ones; [ high; low ]; _; _ |] -> Two_pairs (high, low, ones)
        | [| ones; [ two ]; _; _ |] -> One_pair (two, ones)
        | [| ones; []; []; [] |] -> begin
            match is_straight with
            | Some high -> Straight high
            | _ -> High_card ones
          end
        | _ -> failwith "unrecognized pattern"
  end
end

let parse_line s =
  match String.split_on_char ' ' s |> List.map Poker.Card.of_string with
  | c0 :: c1 :: c2 :: c3 :: c4 :: ([ _; _; _; _; _ ] as rhs) ->
      Poker.Hand.(of_cards [ c0; c1; c2; c3; c4 ], of_cards rhs)
  | _ -> failwith "parse_line: invalid format"

let () =
  In_channel.(input_lines stdin)
  |> List.fold_left
       begin fun acc line ->
         let lhs, rhs = parse_line line in
         (* OCaml's polymorhpic comparison works well here *)
         match compare lhs rhs with
         | x when x > 0 -> acc + 1
         | x when x < 0 -> acc
         | _ -> failwith "tie (shouldn't happen in this problem)"
       end
       0
  |> print_int;
  print_newline ()
