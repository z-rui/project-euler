(** Coin Partitions *)

let find_sub f a first last =
  let rec loop i =
    if i > last then None else if f a.(i) then Some i else loop (i + 1)
  in
  loop first

module type NUM = sig
  type t

  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
end

module Partition_gen (N : NUM) = struct
  (** Partition function generator *)

  (** [cont p first last] fills in p(first..last), assuming that p(0..first-1)
      have been calculated. *)
  let cont p first last =
    for i = first to last do
      (* Make use of generalized pentagonal number optimization:
             p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(12) + p(15) - ...
        References:
        - https://oeis.org/A000041
        - https://oeis.org/A001318
      *)
      let rec loop j k neg sum =
        (* k and k' are the next 2 terms in the gen. pent. num. sequence. *)
        if k <= i then begin
          let k' = k + j in
          let term =
            if k' <= i then begin
              N.add p.(i - k) p.(i - k')
            end
            else p.(i - k)
          in
          let sum' = if neg then N.sub sum term else N.add sum term in
          loop (j + 1) (k' + j + j + 1) (not neg) sum'
        end
        else sum
      in
      p.(i) <- loop 1 1 false N.zero
    done

  (** [search f] returns the first n such that f p(n) is true. *)
  let rec search f =
    let rec aux p first =
      let n = Array.length p in
      cont p first (n - 1);
      match find_sub f p first (n - 1) with
      | Some x -> x
      | None ->
          let p' = Array.make (n * 2) N.zero in
          Array.blit p 0 p' 0 n;
          aux p' n
    in
    let p = Array.make 1000 N.zero in
    p.(0) <- N.one;
    aux p 1
end

let () =
  let module MInt = struct
    include Int

    let m = 1_000_000
    let add a b = (a + b) mod m
    let sub a b = (a - b + m) mod m
  end in
  let module P = Partition_gen (MInt) in
  P.search (fun x -> x = 0) |> print_int;
  print_newline ()
