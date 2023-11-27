module Note : sig
  type naturalnotes = C | D | E | F | G | A | B
  type accidents = FFl | Fl | N | Sh | SSh
  type octave = int
  type note = octave * naturalnotes * accidents

  val shift_natural : naturalnotes -> int -> naturalnotes
  val sharpen : accidents -> accidents
  val flatten : accidents -> accidents
  val int_of_note : note -> int
end = struct
  type naturalnotes = C | D | E | F | G | A | B
  type accidents = FFl | Fl | N | Sh | SSh
  type octave = int
  type note = octave * naturalnotes * accidents

  let int_of_natural n =
  match n with
    A -> 0 | B -> 1 | C -> 2 | D ->3  | E->4  | F->5 | G->6

  let natural_of_int n =
    match n mod 7 with
      0 -> A | 1 -> B | 2 -> C | 3 -> D | 4 -> E | 5 -> F | 6 -> G
      | _ -> failwith "mod out of range"

  let sharpen n =
    match n with
    | FFl -> Fl | Fl -> N | N -> Sh | Sh -> SSh | SSh -> failwith "no SSS"

  let flatten n =
    match n with
    | SSh -> Sh | Sh -> N | N -> Fl | Fl -> FFl | FFl -> failwith "no FFF"

  let shift_natural n d =
    (natural_of_int) (int_of_natural n + d)

  (* TODO: investigate if there's a bug here. There's some weird behaviour of
   [clip_within_one_octave_above_of] where it returns the same octave number
   for SSh and FFl notes that should straddle an octave border:

# clip_within_one_octave_above_of ~bass:Note.(4, D, N) Note.(6,C,SSh) ;;
- : Note.octave * Note.naturalnotes * Note.accidents = (5, Note.C, Note.SSh)
# clip_within_one_octave_above_of ~bass:Note.(4, G, N) Note.(6,A,FFl) ;;
- : Note.octave * Note.naturalnotes * Note.accidents = (5, Note.A, Note.FFl)

   *)
  let int_of_note (o,n,a) =
    let octaviation = 12*o in
    let accidentation = match a with FFl -> -2 | Fl -> -1 | N -> 0 | Sh -> 1 |
    SSh -> 2
    in
    let naturation = match n with
        A -> 0 | B -> 2 | C -> 3 | D -> 5 | E -> 7 | F -> 8 | G -> 10
    in
    naturation+accidentation+octaviation
end



module Interval : sig
  type ordinal =
     | Unison
     | Second
     | Third
     | Fourth
     | Fifth
     | Sixth
     | Seventh
     | Octave
     | Nineth
     | Tenth
     | Eleventh
     | Twelfth
     | Thirteenth
     | Fourteenth
  val ordinal_of_int : int -> ordinal
  val int_of_ordinal : ordinal -> int
  type qualifier = Dim | Min | Perfect | Maj | Aug
  type t = ordinal * qualifier
  val make : ordinal -> qualifier -> t
  val makeint : int -> qualifier -> t
  val inverse : t -> t
end = struct
  type ordinal =
     | Unison
     | Second
     | Third
     | Fourth
     | Fifth
     | Sixth
     | Seventh
     | Octave
     | Nineth
     | Tenth
     | Eleventh
     | Twelfth
     | Thirteenth
     | Fourteenth
  let ordinal_of_int = function
    | 1 -> Unison    
    | 2 -> Second    
    | 3 -> Third     
    | 4 -> Fourth    
    | 5 -> Fifth     
    | 6 -> Sixth     
    | 7 -> Seventh   
    | 8 -> Octave    
    | 9 -> Nineth    
    | 10 ->Tenth     
    | 11 ->Eleventh  
    | 12 ->Twelfth   
    | 13 ->Thirteenth
    | 14 ->Fourteenth
    | _ -> invalid_arg "Interval.ordinal_of_int"
  let int_of_ordinal = function
    | Unison     -> 1
    | Second     -> 2
    | Third      -> 3
    | Fourth     -> 4
    | Fifth      -> 5
    | Sixth      -> 6
    | Seventh    -> 7
    | Octave     -> 8
    | Nineth     -> 9
    | Tenth      -> 10
    | Eleventh   -> 11
    | Twelfth    -> 12
    | Thirteenth -> 13
    | Fourteenth -> 14
  type qualifier = Dim | Min | Perfect | Maj | Aug
  type t = ordinal * qualifier
  let make o q =
    match o with
      | Unison | Fourth | Fifth | Octave | Eleventh | Twelfth ->
         (match q with
            | Dim | Perfect | Aug -> o, q
            | _ -> failwith "no minor/major version of this interval"
         )
      | _ -> (match q with
               Perfect -> failwith "no perfect version of this interval"
               | _ -> o ,q)
  let makeint i q = make (ordinal_of_int i) q
  let inverse_o = function
     | Unison         -> Octave        
     | Second         -> Seventh       
     | Third          -> Sixth         
     | Fourth         -> Fifth         
     | Fifth          -> Fourth        
     | Sixth          -> Third         
     | Seventh        -> Second        
     | Octave         -> Unison        
     | Nineth
     | Tenth
     | Eleventh
     | Twelfth
     | Thirteenth
     | Fourteenth -> failwith "no inverse outside Octave"
  let inverse_q = function
     | Dim -> Aug
     | Min -> Maj
     | Perfect -> Perfect
     | Maj -> Min
     | Aug -> Dim
  let inverse (o, q) =
     (inverse_o o, inverse_q q)
end

(* Eventually we'll need to support aritrary large intervals including > octave *)
let list_intervals_from_int n m =
  match (m - n + 12*9) mod 12 with
    0 -> Interval.[makeint 1 Perfect; makeint 2 Dim; makeint 8 Perfect]
  | 1 -> Interval.[makeint 2 Min; makeint 1 Aug; makeint 8 Aug]
  | 2 -> Interval.[makeint 2 Maj; makeint 3 Dim]
  | 3 -> Interval.[makeint 3 Min; makeint 2 Aug]
  | 4 -> Interval.[makeint 3 Maj; makeint 4 Dim]
  | 5 -> Interval.[makeint 4 Perfect; makeint 3 Aug]
  | 6 -> Interval.[makeint 4 Aug; makeint 5 Dim]
  | 7 -> Interval.[makeint 5 Perfect; makeint 6 Dim]
  | 8 -> Interval.[makeint 6 Min; makeint 5 Aug]
  | 9 -> Interval.[makeint 6 Maj; makeint 7 Dim]
  | 10 -> Interval.[makeint 7 Min; makeint 6 Aug]
  | 11 -> Interval.[makeint 7 Maj; makeint 8 Dim]
  | _ -> failwith "mod failed"
;;


let n : Note.note = 4, D , FFl;;
let h = Note.int_of_note n;;

let m : Note.note = 6, F, N;;
let i = Note.int_of_note m;;

let b = List.hd (list_intervals_from_int h i);;

let add (o, n,a) ( ( ioiq) : Interval.t ) : Note.note =
  let io, _iq = ioiq in
  let m =
     Note.shift_natural
      n
      ((Interval.int_of_ordinal io)-1) in
  let h = Note.int_of_note (o,n,a) in
  let accidents = Note.[FFl; Fl; N ; Sh; SSh] in
  let a =
    List.find
      ( fun a ->
        let m = (o, m, a) in
        let i = Note.int_of_note m in
        List.mem ioiq (list_intervals_from_int h i))
      accidents
  in
  (o, m, a)
;;

add (n) b;;

let add_octave (o, n, a) = (o+1, n, a);;
let rec add_octaves n k =
   if k <= 0 then n else add_octaves (add_octave n) (k - 1);;

module Voices : sig
   type t = Soprane | Alto | Tenor | Bass
   val in_range : t -> Note.note -> int (* neg/0/pos for too low / ok / too high *)
end = struct
   type t = Soprane | Alto | Tenor | Bass
   let lowest = function
      | Soprane -> Note.int_of_note (4, C, N)
      | Alto -> Note.int_of_note (3, G, N)
      | Tenor -> Note.int_of_note (3, C, N)
      | Bass -> Note.int_of_note (2, G, N)
   let highest v = lowest v + 24
   let in_range v n =
      let n = Note.int_of_note n in
      let l = lowest v in
      if n < l then
         n - l
      else
         let h = highest v in
         if h < n then
            n - h
         else
            0
end ;;

Voices.(in_range Soprane Note.(2, G, N)) ;;
Voices.(in_range Soprane Note.(4, G, SSh)) ;;

module Chord : sig
   type name = Maj5 | Min5 | Dim5
   type t = {
      bass: Note.note;
      name: name;
   }
   val make : Note.note -> name -> t
   val more_notes : t -> Note.note Seq.t
   val voicings : t -> (Note.note * Note.note * Note.note) Seq.t
end = struct
   type name = Maj5 | Min5 | Dim5
   type t = {
      bass: Note.note;
      name: name;
   }
   let make bass name = { bass; name }

   let voicings { bass; name } =
      let (low1, low2) =
         match name with
         | Maj5 -> add bass Interval.(makeint 3 Maj),
                   add bass Interval.(makeint 5 Perfect)
         | Min5 -> add bass Interval.(makeint 3 Min),
                   add bass Interval.(makeint 5 Perfect)
         | Dim5 -> add bass Interval.(makeint 3 Min),
                   add bass Interval.(makeint 5 Dim )
      in
   let voice (bass, low1, low2) (ob, o1, o2) =
      (add_octaves bass ob,
       add_octaves low1 o1,
       add_octaves low2 o2)
   in
   let one_choice = Seq.ints 0 |> Seq.take 10 (* we work in 10 octaves *) in
   let three_choices =
      Seq.map
        (fun ((a, b), c) -> (a, b, c))
        (
      Seq.product
        (Seq.product one_choice one_choice) one_choice)
   in
   Seq.map (voice (bass, low1, low2)) three_choices

   let more_notes { bass; name } =
      let low_notes =
      Seq.map
        (add bass)
        (
         match name with
         | Maj5 -> List.to_seq Interval.[ makeint 1 Perfect; makeint 3 Maj; makeint 5 Perfect ]
         | Min5 -> List.to_seq Interval.[ makeint 1 Perfect; makeint 3 Min; makeint 5 Perfect ]
         | Dim5 -> List.to_seq Interval.[ makeint 1 Perfect; makeint 3 Min; makeint 5 Dim ]
        )
      in
      let octaves =
         Seq.ints 0
            |> Seq.take 10 (* we only take 10 octaves *)
      in
      Seq.concat
        (Seq.map
          (fun o -> Seq.map (fun n -> add_octaves n o) low_notes)
          octaves
          )
end;;

let v = Chord.voicings Chord.(make Note.(4, C, N) Dim5) ;;

let clip_within_one_octave_above_of ~bass other =
   let (bo, bn, _) = bass in
   let (_, on, oa) = other in
   let shifted_other = (bo, on, oa) in
   let cmp = compare
     (Note.int_of_note shifted_other) (Note.int_of_note bass)
     in
   if cmp < 0 then
      (bo+1, on, oa)
   else if cmp > 0 then
      (bo, on, oa)
   else (*if cmp = 0 then*)
      if on = bn then
         (bo, on, oa)
      else
      failwith "don't know what to do yet"
;;

(* clip_within_one_octave_above_of ~bass:Note.(4,C,N) Note.(6,B,Sh);; *)
