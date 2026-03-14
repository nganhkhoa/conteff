
let [@contract : bigger_10] x = 1
let x = 2

let y = 2
let [@contract : bigger_10] y = 1

(* will not fix because second x is not a contract *)
let use_x = x + 1
(* fix because second y is a contract *)
let use_y = y + 1
