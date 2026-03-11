open Effect.Deep

exception Blame of string

let [@contract]
  single_value
  : int [@post : bigger_than_40] =
  x

