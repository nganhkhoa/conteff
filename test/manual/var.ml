open Effect.Deep

exception Blame of string

let bigger_than_40 x = x > 40

let x = 50

let [@contract : bigger_than_10]
  single_value : int =
  x

let [@contract : bigger_than_20]
  with_nested : int =
  let [@contract : bigger_than_40] ohwao : int = 10 in
  x + ohwao
  (* x + (ohwao "?" "?" "?") *)
