open Conteff_lib

(* Both values are threaded by the same contract-aware handler. *)
[%%contract_states
let calls : int = 0
and last_argument : string = ""]

let record_call argument =
  [%contract_set_calls ([%contract_get_calls] + 1)];
  [%contract_set_last_argument argument];
  true

let[@contract record_call @-> Contract.any] greet name = "Hello, " ^ name

let () =
  [%contract_main_states]
    (let first = greet "Ada" in
     let second = greet "Grace" in
     let calls = [%contract_get_calls] in
     let last = [%contract_get_last_argument] in
     assert (calls = 2 && last = "Grace");
     Printf.printf "Greetings: %s; %s\nCalls: %d\nLast argument: %s\n"
       first second calls last)
