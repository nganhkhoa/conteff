open Conteff_lib

let any _ = true
let bigger_10 x = x > 10
let smaller_20 x = x < 20

(* so box is a tuple of functions
   placing contracts on function is possible
   placing contracts on tuple is also possible
 *)
let process (box1 : int Box.t) (box2 : int Box.t) =
  let v1 = Box.get box1 in
  let _ = Box.set box1 (v1 + 100) in
  let v3 = Box.get box1 in
  Printf.printf "box1: before=%d after=%d\n" v1 v3;

  let v1 = Box.get box2 in
  let _ = Box.set box2 (v1 * 100) in
  let v3 = Box.get box2 in
  Printf.printf "box2: before=%d after=%d\n" v1 v3

let main () =
  let (box1_handler, box1) = Box.create 0 in
  let (box2_handler, box2) = Box.create 1000 in
  box2_handler (fun () ->
    box1_handler (fun () ->
      process box1 box2))

let () =
  main ()
