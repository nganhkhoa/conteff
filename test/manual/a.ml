open Effect.Deep

exception Blame of string

(* the rewriter must be able to distinguish between
   arguments and local variables of the same name

   because the rules will rewrite the arguments into
   local variables too, we need to be careful
 *)
let [@contract : k1 -> k2 -> k3] f (x : t1) (y : t2): t3 =
  let [@contract : k4] x = 1 in
  x + y
