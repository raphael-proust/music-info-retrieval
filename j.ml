let j = Yojson.Basic.from_file "/dev/stdin"

let note_to_val = function
| "A0"|"G##0"|"B♭♭0" -> 0
| "B♭0"|"A#0"|"C♭♭0" -> 1
| "B0"|"C♭0"|"A##0" -> 2
| "C0"|"B#0"|"D♭♭0" -> 3
| "C#0"|"D♭0"|"B##0" -> 4
| "D0"|"E♭♭0"|"C##0" -> 5
| "E♭0"|"D#0"|"F♭♭0" -> 6
| "E0"|"F♭0"|"D##0" -> 7
| "F0"|"E#0"|"G♭♭0" -> 8
| "F#0"|"G♭0"|"E##0" -> 9
| "G0"|"F##0"|"A♭♭0" -> 10
| "G#0"|"A♭0" -> 11
| "A1"|"G##1"|"B♭♭1" -> 12
| "B♭1"|"A#1"|"C♭♭1" -> 13
| "B1"|"C♭1"|"A##1" -> 14
| "C1"|"B#1"|"D♭♭1" -> 15
| "C#1"|"D♭1"|"B##1" -> 16
| "D1"|"E♭♭1"|"C##1" -> 17
| "E♭1"|"D#1"|"F♭♭1" -> 18
| "E1"|"F♭1"|"D##1" -> 19
| "F1"|"E#1"|"G♭♭1" -> 20
| "F#1"|"G♭1"|"E##1" -> 21
| "G1"|"F##1"|"A♭♭1" -> 22
| "G#1"|"A♭1" -> 23
| "A2"|"G##2"|"B♭♭2" -> 24
| "B♭2"|"A#2"|"C♭♭2" -> 25
| "B2"|"C♭2"|"A##2" -> 26
| "C2"|"B#2"|"D♭♭2" -> 27
| "C#2"|"D♭2"|"B##2" -> 28
| "D2"|"E♭♭2"|"C##2" -> 29
| "E♭2"|"D#2"|"F♭♭2" -> 30
| "E2"|"F♭2"|"D##2" -> 31
| "F2"|"E#2"|"G♭♭2" -> 32
| "F#2"|"G♭2"|"E##2" -> 33
| "G2"|"F##2"|"A♭♭2" -> 34
| "G#2"|"A♭2" -> 35
| "A3"|"G##3"|"B♭♭3" -> 36
| "B♭3"|"A#3"|"C♭♭3" -> 37
| "B3"|"C♭3"|"A##3" -> 38
| "C3"|"B#3"|"D♭♭3" -> 39
| "C#3"|"D♭3"|"B##3" -> 40
| "D3"|"E♭♭3"|"C##3" -> 41
| "E♭3"|"D#3"|"F♭♭3" -> 42
| "E3"|"F♭3"|"D##3" -> 43
| "F3"|"E#3"|"G♭♭3" -> 44
| "F#3"|"G♭3"|"E##3" -> 45
| "G3"|"F##3"|"A♭♭3" -> 46
| "G#3"|"A♭3" -> 47
| "A4"|"G##4"|"B♭♭4" -> 48
| "B♭4"|"A#4"|"C♭♭4" -> 49
| "B4"|"C♭4"|"A##4" -> 50
| "C4"|"B#4"|"D♭♭4" -> 51
| "C#4"|"D♭4"|"B##4" -> 52
| "D4"|"E♭♭4"|"C##4" -> 53
| "E♭4"|"D#4"|"F♭♭4" -> 54
| "E4"|"F♭4"|"D##4" -> 55
| "F4"|"E#4"|"G♭♭4" -> 56
| "F#4"|"G♭4"|"E##4" -> 57
| "G4"|"F##4"|"A♭♭4" -> 58
| "G#4"|"A♭4" -> 59
| "A5"|"G##5"|"B♭♭5" -> 60
| "B♭5"|"A#5"|"C♭♭5" -> 61
| "B5"|"C♭5"|"A##5" -> 62
| "C5"|"B#5"|"D♭♭5" -> 63
| "C#5"|"D♭5"|"B##5" -> 64
| "D5"|"E♭♭5"|"C##5" -> 65
| "E♭5"|"D#5"|"F♭♭5" -> 66
| "E5"|"F♭5"|"D##5" -> 67
| "F5"|"E#5"|"G♭♭5" -> 68
| "F#5"|"G♭5"|"E##5" -> 69
| "G5"|"F##5"|"A♭♭5" -> 70
| "G#5"|"A♭5" -> 71
| "A6"|"G##6"|"B♭♭6" -> 72
| "B♭6"|"A#6"|"C♭♭6" -> 73
| "B6"|"C♭6"|"A##6" -> 74
| "C6"|"B#6"|"D♭♭6" -> 75
| "C#6"|"D♭6"|"B##6" -> 76
| "D6"|"E♭♭6"|"C##6" -> 77
| "E♭6"|"D#6"|"F♭♭6" -> 78
| "E6"|"F♭6"|"D##6" -> 79
| "F6"|"E#6"|"G♭♭6" -> 80
| "F#6"|"G♭6"|"E##6" -> 81
| "G6"|"F##6"|"A♭♭6" -> 82
| "G#6"|"A♭6" -> 83
| "A7"|"G##7"|"B♭♭7" -> 84
| "B♭7"|"A#7"|"C♭♭7" -> 85
| "B7"|"C♭7"|"A##7" -> 86
| "C7"|"B#7"|"D♭♭7" -> 87
| "C#7"|"D♭7"|"B##7" -> 88
| "D7"|"E♭♭7"|"C##7" -> 89
| "E♭7"|"D#7"|"F♭♭7" -> 90
| "E7"|"F♭7"|"D##7" -> 91
| "F7"|"E#7"|"G♭♭7" -> 92
| "F#7"|"G♭7"|"E##7" -> 93
| "G7"|"F##7"|"A♭♭7" -> 94
| "G#7"|"A♭7" -> 95
| s -> failwith ("Unknown note: " ^s)

let note prev : Yojson.Basic.t -> int = function
   | `Assoc assoc -> begin
      let t =
         match
         List.assoc "ticks" assoc
         with
         | `Int i -> string_of_int i
         | _ -> assert false
      in
      let d =
         match
         List.assoc "durationTicks" assoc
         with
         | `Int i -> string_of_int i
         | _ -> assert false
      in
      let v =
         match
         List.assoc "name" assoc
         with
         | `String s -> note_to_val s
         | _ -> assert false
      in
      Printf.printf "%s %s %d\n" t d (v - prev);
      v
   end
   | _ -> assert false

let notes = function
   | `List [] -> ()
   | `List (t::ts) ->
         let first = note 0 t in
         let _ = List.fold_left note first ts in
         ()
   | _ -> assert false

let track = function
   | `Assoc assoc -> notes (List.assoc "notes" assoc)
   | _ -> assert false

let tracks = function
   | `List ts -> List.iter track ts
   | _ -> assert false

let f = function
   | `Assoc assoc -> tracks (List.assoc "tracks" assoc)
   | _ -> assert false

let () = f j
