let j = Yojson.Basic.from_file "/dev/stdin"

let () = Jlib.f j

(* force to compile harmonie *)

include Harmonie
