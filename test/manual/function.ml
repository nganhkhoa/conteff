open Effect.Deep

exception Blame of string

let
  [@contract : a -> b]
  [@effect : effect_A -> check_effectA_return ]
  run x : int =
  x + 1
  (* (x neg pos "run.x") + 1 *)

let
  [@contract : (a -> b) -> c]
  [@effect : effect_A -> check_effectA_return ]
  run f : int =
  f 1
  (* (f neg pos "run.x") + 1 *)

let
  [@contract : a -> (b -> 'd c)]
  [@effect : effect_A -> check_effectA_return ]
  run x : int =
  let f y = x + y in
  f 1
