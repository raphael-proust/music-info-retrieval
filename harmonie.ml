module Note : sig
  type naturalnotes = A | B | C | D | E | F | G
  type accidents = FF | F | N | S | SS
  type octave = int
  type note = naturalnotes * accidents * octave

  val shift_natural : naturalnotes -> int -> naturalnotes
  val sharpen : accidents -> accidents
  val flatten : accidents -> accidents
  val int_of_note : note -> int
end = struct
  type naturalnotes = A | B | C | D | E | F | G
  type accidents = FF | F | N | S | SS
  type octave = int
  type note = naturalnotes * accidents * octave

  let int_of_natural n =
  match n with
    A -> 0 | B -> 1 | C -> 2 | D ->3  | E->4  | F->5 | G->6

  let natural_of_int n =
    match n mod 7 with
      0 -> A | 1 -> B | 2 -> C | 3 -> D | 4 -> E | 5 -> F | 6 -> G
      | _ -> failwith "mod out of range"

  let sharpen n =
    match n with
    | FF -> F | F -> N | N -> S | S -> SS | SS -> failwith "no SSS"

  let flatten n =
    match n with
    | SS -> S | S -> N | N -> F | F -> FF | FF -> failwith "no FFF"

  let shift_natural n d =
    (natural_of_int) (int_of_natural n + d)

  let int_of_note (n,a,o) =
    let octaviation = 7*o
    in
    let accidentation = match a with FF -> -2 | F -> -1 | N -> 0 | S -> 1 | SS -> 2
    in
    let naturation = match n with
        A -> 0 | B -> 2 | C -> 3 | D -> 5 | E -> 7 | F -> 8 | G -> 10
    in
    naturation+accidentation+octaviation
end



module Interval : sig
  type ordinal = private int
  val ordinal_of_int : int -> ordinal
  type qualifier = Dim | Min | Perfect | Maj | Aug
  type t = ordinal * qualifier
  val make : int -> qualifier -> t
end = struct
  type ordinal = int
  let ordinal_of_int o =
    if o <= 0 then failwith "ordinal of int range error"
    else o
  type qualifier = Dim | Min | Perfect | Maj | Aug
  type t = ordinal * qualifier
  let make o q =
    match o with
      | 1 | 4 | 5 | 8 ->
         (match q with
            | Dim | Perfect | Aug -> o, q
            | _ -> failwith "no minor/major version of this interval"
         )
      | _ -> (match q with
               Perfect -> failwith "no perfect version of this interval"
               | _ -> o ,q)
end

let list_intervals_from_int n m =
  match (n - m + 12*9) mod 12 with
    0 -> Interval.make 1 Perfect
  | 1 -> Interval.make 2 Min
  | 2 -> Interval.make 2 Maj
  | 3 -> Interval.make 3 Min
  | 4 -> Interval.make 3 Maj
  | 5 -> Interval.make 4 Perfect
  | 6 -> Interval.make 4 Aug
  | 7 -> Interval.make 5 Perfect
  | 8 -> Interval.make 6 Min
  | 9 -> Interval.make 6 Maj
  | 10 -> Interval.make 7 Min
  | 11 -> Interval.make 7 Maj
  | _ -> failwith "mod failed"
;;


let n : Note.note = D , FF , 4;;
let h = Note.int_of_note n;;

let m : Note.note = F, N , 6;;
let i = Note.int_of_note m;;

let b = list_intervals_from_int h i ;;

let add (n,a,o) ( ( ioiq) : Interval.t ) : Note.note =
  let io,iq = ioiq in
  let m = (Note.shift_natural n ((io:>int)-1)) in
  let h = Note.int_of_note (n,a,o) in
  let accidents = Note.[FF; F; N ; S; SS] in
  let a =
    List.find
      ( fun a ->
        let m = (m, a, o) in
        let i = Note.int_of_note m in
        list_intervals_from_int h i = (ioiq) )
      accidents
  in
  (m, a, o)
;;

add (n) b;;


(*
let a =
  match iq with
  | Dim -> flatten ( flatten a ) )
  | Min -> flatten a
  | Perfect -> a
  | Maj -> 
*)
