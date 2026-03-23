(** Monopoly Odds *)

module Monopoly = struct
  type pos = int
  type card_action = No_action | Goto of pos | Back of int | Next_R | Next_U

  let total_pos = 40
  let jail_pos = 10

  type card_pile = { mutable cntr : int; cards : card_action array }

  let create_card_pile size cards =
    let pile = { cntr = 0; cards = Array.make size No_action } in
    Array.blit cards 0 pile.cards 0 (Array.length cards);
    for i = 0 to size - 1 do
      let j = Random.int_in_range ~min:i ~max:(size - 1) in
      let tmp = pile.cards.(i) in
      pile.cards.(i) <- pile.cards.(j);
      pile.cards.(j) <- tmp
    done;
    pile

  let draw_card pile =
    let card = pile.cards.(pile.cntr) in
    pile.cntr <- (pile.cntr + 1) mod Array.length pile.cards;
    card

  type state = {
    mutable pos : pos;
    mutable double_dice_cntr : int;
    community_chest : card_pile;
    chance : card_pile;
  }

  let create_state () =
    {
      pos = 0;
      double_dice_cntr = 0;
      community_chest = create_card_pile 16 [| Goto 0 (* GO *); Goto jail_pos |];
      chance =
        create_card_pile 16
          [|
            Goto 0 (* GO *);
            Goto jail_pos;
            Goto 11 (* C1 *);
            Goto 24 (* E3 *);
            Goto 39 (* H2 *);
            Goto 5 (* R1 *);
            Next_R;
            Next_R;
            Next_U;
            Back 3;
          |];
    }

  let rec apply_card state card =
    match card with
    | No_action -> ()
    | Goto pos -> state.pos <- pos
    | Next_R ->
        let r_pos =
          match Char.chr state.pos with
          | '\035' .. '\039' | '\000' .. '\004' -> 5
          | '\005' .. '\014' -> 15
          | '\015' .. '\024' -> 25
          | '\025' .. '\034' -> 35
          | _ -> assert false
        in
        state.pos <- r_pos
    | Next_U ->
        let u_pos = if state.pos >= 12 && state.pos <= 28 then 28 else 12 in
        state.pos <- u_pos
    | Back n -> move state (-n)

  and move state steps =
    state.pos <- (state.pos + steps + total_pos) mod total_pos;
    match state.pos with
    | 30 (* G2J *) -> state.pos <- jail_pos
    | 2 | 17 | 33 (* CC *) -> apply_card state (draw_card state.community_chest)
    | 7 | 22 | 36 (* CH *) -> apply_card state (draw_card state.chance)
    | _ (* locations with no action *) -> ()

  let roll_dice ?(sides = 6) state =
    let d1 = Random.int_in_range ~min:1 ~max:sides
    and d2 = Random.int_in_range ~min:1 ~max:sides in
    if d1 = d2 then
      if state.double_dice_cntr = 2 then begin
        state.double_dice_cntr <- 0;
        state.pos <- jail_pos (* In jail now; do not move *)
      end
      else begin
        state.double_dice_cntr <- state.double_dice_cntr + 1;
        move state (d1 + d2)
      end
    else begin
      state.double_dice_cntr <- 0;
      move state (d1 + d2)
    end

  type stats = { pos_cntr : int array; mutable total_steps : int }

  let create_stats () = { pos_cntr = Array.make total_pos 0; total_steps = 0 }

  let update_stats state stats =
    stats.pos_cntr.(state.pos) <- stats.pos_cntr.(state.pos) + 1;
    stats.total_steps <- stats.total_steps + 1
end

let print_stats ({ pos_cntr; total_steps } : Monopoly.stats) =
  let freq =
    Array.init Monopoly.total_pos (fun i ->
        (Float.of_int pos_cntr.(i) /. Float.of_int total_steps, i))
  in
  Array.sort (Fun.flip compare) freq;
  for i = 0 to 4 do
    let f, idx = freq.(i) in
    Printf.printf "%02d (%.02f%%) " idx (f *. 100.)
  done;
  print_newline ()

let () =
  Random.self_init ();
  let stats = Monopoly.create_stats () in
  let simulation_steps = 10_000_000 in
  let state = Monopoly.create_state () in
  for step = 1 to simulation_steps do
    Monopoly.roll_dice ~sides:4 state;
    Monopoly.update_stats state stats;
    if step mod 1_000_000 = 0 then begin
      Printf.printf "%10d: " step;
      print_stats stats
    end
  done
