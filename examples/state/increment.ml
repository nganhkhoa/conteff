open Conteff_lib

(* The domain predicate always accepts and increments the call count.
   No reference cell is used: the shallow handler threads the state. *)
let count_call _ =
  Contract.set_state (Contract.get_state () + 1);
  true

let[@contract count_call @-> Contract.any] increment x = x + 1

let () =
  [%contract_main_state 0]
    (let a = increment 10 in
     let b = increment 20 in
     let calls = Contract.get_state () in
     Printf.printf "Results: (%d, %d)\nCalls: %d\n" a b calls)
