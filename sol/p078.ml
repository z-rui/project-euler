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

  type t = N.t Dynarray.t

  let create () : t = Dynarray.make 1 N.one

  let next p : N.t =
    let i = Dynarray.length p in
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
          if k' <= i then
            N.add (Dynarray.get p (i - k)) (Dynarray.get p (i - k'))
          else Dynarray.get p (i - k)
        in
        let sum' = if neg then N.sub sum term else N.add sum term in
        loop (j + 1) (k' + j + j + 1) (not neg) sum'
      end
      else sum
    in
    let result = loop 1 1 false N.zero in
    Dynarray.add_last p result;
    result

  let seq : N.t Seq.t =
   fun () -> Seq.Cons (N.one, Seq.unfold (fun p -> Some (next p, p)) (create ()))
end

let () =
  let module MInt = struct
    include Int

    let m = 1_000_000
    let add a b = (a + b) mod m
    let sub a b = (a - b + m) mod m
  end in
  let module P = Partition_gen (MInt) in
  Seq.find_index (fun x -> x = 0) P.seq |> Option.get |> print_int;
  print_newline ()
