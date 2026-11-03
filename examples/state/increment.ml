open Conteff_lib

(* The domain predicate always accepts and increments the call count.
   No reference cell is used: the shallow handler threads the state. *)
let count_call _ =
  Contract.set_state (Contract.get_state () + 1);
  true

let[@contract count_call @-> Contract.any] increment x = x + 1

let () =
  let (a, b), calls = Contract.with_state_result ~initial:0 (fun () ->
    let a = increment 10 in
    let b = increment 20 in
    (a, b)) in
  Printf.printf "Results: (%d, %d)\nCalls: %d\n" a b calls
