open Conteff_lib

let checking_g'p1 v =
  Printf.printf "\tchecking g'p1 v=%d uwu=%d\n" v 1;
  true

let checking_g'p2'p1 v =
  Printf.printf "\tchecking g'p2'p1 v=%d uwu=%d\n" v 2;
  true

let checking_g'p2'r v =
  Printf.printf "\tchecking g'p2'r v=%d uwu=%d\n" v 3;
  true

let checking_g'r p1 p2 v =
  Printf.printf "\tchecking g'r p1=%d v=%d uwu=%d\n" p1 v 4;
  let _ = p2 222 in
  true

let checking_x v =
  Printf.printf "\tchecking x v=%d uwu=%d\n" v 5;
  true

let checking_r g x v =
  Printf.printf "\tchecking r x=%d v=%d uwu=%d\n" x v 6;
  Printf.printf "\t\tdependent call to argument\n";
  let _ = g v (fun y -> y + 1000) in
  true


let
  [@contract
  : ( ((checking_g'p1 * (checking_g'p2'p1 -> checking_g'p2'r)) -> checking_g'r as 'dep)
    * checking_x)
  -> checking_r as 'dep]
  f (g : int -> (int -> int) -> int) (x : int) : int =
    let h = fun x ->
      x + 100
    in
    g x h

let main = fun () ->
  let g = fun x h ->
    h (x + 10)
  in

  (* handler for main effects, effects called during f execution *)

  let pos = "f" in
  let neg = "main" in
  let cloc = "f" in
  let f = f pos neg cloc in

  let r = f g 1 in
  Printf.printf "result is %d\n" r

let () =
  Contract.run_with_effects main
