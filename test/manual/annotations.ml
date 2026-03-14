(* test to see if the parser accepts these annotations *)

let body = 1

(* flat contract *)
let [@contract : predicate] x = body

(* function contract *)
let [@contract : predicate -> predicate] f1 = body

(* higher order contract *)
let [@contract : (predicate -> predicate) -> predicate] f2 = body

(* dependent contract *)
let [@contract : predicate -> predicate dep] f3 = body

(* do we have this? *)
let [@contract : predicate -> predicate dep -> predicate dep] f4 = body

(* trace contract *)
let [@contract : (constructor, predicate) trace] x1 = body

(* TODO *)

(* main-effect contract *)
(* contract-handler contract *)
