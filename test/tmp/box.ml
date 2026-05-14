open Conteff_lib

let any _ = true
let bigger_10 x = x > 10
let smaller_20 x = x < 20

(* so box is a tuple of functions
   placing contracts on function is possible
   placing contracts on tuple is also possible
 *)
let [@contract
  : (((any -> smaller_20) * (any -> any) as 'tup)
  * ((any -> any) * (any -> any) as 'tup))
  -> any]
  process (box1 : int Box.t) (box2 : int Box.t) : unit =
  let v1 = Box.get box1 in
  Printf.printf "box1: before=%d\n" v1;
  let _ = Box.set box1 (v1 + 100) in
  let v3 = Box.get box1 in
  Printf.printf "box1: after=%d\n" v3;

  (* let v1 = Box.get box2 in
  let _ = Box.set box2 (v1 * 100) in
  let v3 = Box.get box2 in
  Printf.printf "box2: before=%d after=%d\n" v1 v3; *)
  ()

let main () =
  let pos = "process" in
  let neg = "main" in
  let cloc = "process" in
  let process = process pos neg cloc in

  let (box1_handler, box1) = Box.create 0 in
  let (box2_handler, box2) = Box.create 1000 in
  box2_handler (fun () ->
    box1_handler (fun () ->
      process box1 box2))

let () =
  Contract.run_with_effects main
