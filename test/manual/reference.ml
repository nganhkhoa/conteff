(* let ThisModule denotes this module name *)

let [@contract : bigger_10] x = 9

let [@contract : bigger_10] y = 9

let use_x = x + 1

(* rewritten into

   let use_x = (x "ThisModule" __FUNCTION__ "ThisModule.x") + 1
 *)

(* if use_x is defined in another module
   the above rewritten holds
 *)

let [@contract : bigger_10 -> bigger_10] f x = x + y

(* rewritten into

  1st phase: bootstrap

  let [@contract : bigger_10 -> bigger_20] f pos neg cloc x =
    let [@contract : bigger_10] x = x in
    let x = x in
    let [@contract : bigger_20] ret = x + y in
    ret

  2nd phase: references fix

  let [@contract : bigger_10 -> bigger_20] f pos neg cloc x =
    let [@contract : bigger_10] x = x in
    let x = (x neg pos cloc) in
    let [@contract : bigger_20] ret = x + (y "ThisModule" __FUNCTION__ "ThisModule.y") in
    ret
 *)

let [@contract : bigger_10 -> bigger_20 dep] f x = x + y

(* rewritten into

   let f pos neg cloc x =
     let [@contract : bigger_10] x = x in
     let x = x in
     let __ret_contract__ = bigger_20 (x neg cloc cloc) in
     let [@contract : __ret_contract__] __ret__ = x + x in
     ret

   let f pos neg cloc x =
     let [@contract : bigger_10] x = x in
     let x = (x neg pos cloc) in
     let __ret_contract__ = bigger_20 (x neg cloc cloc) in
     let [@contract : __ret_contract__] __ret__ = x + x in
     ret

 *)

module A = struct
let [@contract : k1] x =
  let [@contract : k2] y = value in
  let f z = z + y in
  f 0
end
