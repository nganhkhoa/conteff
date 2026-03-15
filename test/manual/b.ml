open Effect.Deep

exception Blame of string


let [@contract : (k1 -> k2) -> k3] f (g : t1 -> t2) : t3 =
  g 0

let () =
  let foo (x : t1) : t2 = y in
  f foo
