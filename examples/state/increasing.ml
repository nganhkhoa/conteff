open Conteff_lib

(* [None] means there has not yet been an accepted input. *)
[%%contract_states
let previous : int option = None]

(* Reject a smaller input; failed checks leave the previous accepted input intact. *)
let nondecreasing input =
  match [%contract_get_previous] with
  | Some previous when input < previous -> false
  | _ ->
      [%contract_set_previous (Some input)];
      true

let[@contract nondecreasing @-> Contract.any] process input = input * 2

let () =
  [%contract_main_states]
    (List.iter
       (fun input ->
         try Printf.printf "%d -> %d\n" input (process input)
         with Contract.Blame _ ->
           Printf.printf "%d -> rejected (smaller than previous accepted input)\n" input)
       [ 1; 3; 3; 2; 5 ])
