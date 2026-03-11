open Effect.Deep

exception Blame of string

let
  [@contract]
  [@effect : effect_A -> check_effectA_return ]
  run
  (x : int [@pre : bigger_than_10])
  : int [@post : bigger_than_20] =
  x + 1
