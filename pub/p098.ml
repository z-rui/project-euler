(** Anagramic Squares *)

let digit_char d = Char.chr (Char.code '0' + d)

let sort_chars s =
  let len = String.length s in
  let chars = Array.init len (fun i -> (s.[i], i)) in
  Array.fast_sort (Pair.compare Char.compare Int.compare) chars;
  (String.init len (fun i -> fst chars.(i)), Array.map snd chars)

let unpermute_chars key perm =
  let len = String.length key in
  let buf = Bytes.create len in
  Array.iteri (fun i j -> Bytes.set buf j key.[i]) perm;
  Bytes.unsafe_to_string buf

let permute_chars s perm =
  String.init (Array.length perm) (fun i -> s.[perm.(i)])

let recode s =
  String.init (String.length s) (fun i -> digit_char (String.index s s.[i]))

module H = Hashtbl.Make (String)

(* group of anagramic squares, keyed by sorted string representation *)
let make_anagram_table sqrt_n =
  let anagram_table = H.create 1000 in
  for i = 1 to sqrt_n do
    let sq = i * i in
    let key, perm = sort_chars (Int.to_string sq) in
    let squares =
      match H.find_opt anagram_table key with
      | None ->
          let squares = Dynarray.create () in
          H.add anagram_table key squares;
          squares
      | Some squares -> squares
    in
    Dynarray.add_last squares (sq, perm)
  done;
  H.filter_map_inplace
    (fun _ squares ->
      if Dynarray.length squares < 2 then None else Some squares)
    anagram_table;
  Printf.eprintf "anagram_table has %d entries\n%!" (H.length anagram_table);
  anagram_table

let print_perm chan perm = Array.iter (Printf.fprintf chan "%d") perm

let solve words =
  let words = List.filter (fun s -> String.length s <= 10) words in
  Printf.eprintf "%d filtered words\n%!" (List.length words);
  let words_table = H.create 1000 in
  List.iter (fun word -> H.add words_table word ()) words;

  let anagram_table = make_anagram_table 99_999 in

  let pattern_table = H.create 1000 in
  H.iter
    begin fun key ->
      Dynarray.iter @@ fun (sq, perm) ->
      let pattern = recode (Int.to_string sq) in
      H.add pattern_table pattern (key, sq, perm)
    end
    anagram_table;
  Printf.eprintf "pattern_table has %d entries\n%!" (H.length pattern_table);

  let max_sq = ref 0 in
  List.iter
    begin fun word ->
      H.find_all pattern_table (recode word)
      |> List.iter @@ fun (key, sq, perm) ->
         if sq > !max_sq then
           let preword = permute_chars word perm in
           H.find anagram_table key
           |> Dynarray.iter @@ fun (sq', perm') ->
              if sq > sq' && H.mem words_table (unpermute_chars preword perm')
              then max_sq := sq
    end
    words;
  !max_sq

let () =
  In_channel.(input_all stdin)
  |> String.trim |> Euler.Word_file.parse |> solve |> print_int;
  print_newline ()
