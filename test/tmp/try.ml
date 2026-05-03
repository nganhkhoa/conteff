exception Blame of string

let pos = "f"
let neg = "main"
let cloc = "f"

let checking_g'p1 v =
  Printf.printf "\tchecking g'p1 v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let checking_g'p2'p1 v =
  Printf.printf "\tchecking g'p2'p1 v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let checking_g'p2'r v =
  Printf.printf "\tchecking g'p2'r v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let checking_g'r p1 p2 v =
  Printf.printf "\tchecking g'r p1=%d v=%d uwu=%d\n" p1 v (Effect.perform Getstate);
  let uhuh = Effect.perform (Dosomething 2) in
  let _ = p2 uhuh in
  true

let checking_x v =
  Printf.printf "\tchecking x v=%d uwu=%d\n" v (Effect.perform Getstate);
  let uhuh = Effect.perform (Dosomething 1) in
  true

let checking_r g x v =
  Printf.printf "\tchecking r x=%d v=%d uwu=%d\n" x v (Effect.perform Getstate);
  Printf.printf "\t\tdependent call to argument\n";
  let _ = g v (fun y -> y + 1000) in
  true


let
  [@contract
  : ((uhuh_do_check
      -> (checking_g'p2'p1 -> checking_g'p2'r)
      -> checking_g'r) dep
  -> checking_x
  -> checking_r) dep]
  (* [@effect_contract : (checking_runsomeeffect_arg -> checking_runsomeeffect) as 'Runsomeeffect] *)
  (* [@contract_handler : contract_effect_handler * contract_state * initial_contract_state] (* what about more than one? keep nesting them? *) *)
  f (g : int -> (int -> int) -> int) (x : int) : int =
    let h = fun x ->
      x + 100
    in
    g x h

let () =
  let g = fun x h ->
    h (x + 10)
  in

  (* handler for main effects, effects called during f execution *)
  let r = f g 1 in
  Printf.printf "result is %d\n" r

