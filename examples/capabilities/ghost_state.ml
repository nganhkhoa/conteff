(* Ghost state records the order of calls in a simple A-then-B protocol.
   Run with: dune exec examples/capabilities/ghost_state.exe *)
open Conteff_lib

[%%contract_states
let has_a_called : bool = false
and has_b_called : bool = false]

(* A may be called until B has been called. A successful check records A. *)
let before_b _argument =
  if [%contract_get_has_b_called] then false
  else ([%contract_set_has_a_called true]; true)

(* B requires a previous A. A successful check records B. *)
let after_a _argument =
  if [%contract_get_has_a_called] then
    ([%contract_set_has_b_called true]; true)
  else false

let[@contract before_b @-> Contract.any] a () = ()
let[@contract after_a @-> Contract.any] b () = ()

(* The final range contract requires both steps, then resets the ghost state
   for the next protocol run. It runs only when the body returns normally. *)
let finish _result =
  let complete = [%contract_get_has_a_called] && [%contract_get_has_b_called] in
  [%contract_set_has_a_called false];
  [%contract_set_has_b_called false];
  complete

let[@contract Contract.any @-> finish] protocol body = body ()

let expect_blame thunk =
  match thunk () with
  | _ -> failwith "expected contract blame"
  | exception Contract.Blame _ -> ()

let clear () =
  assert (not [%contract_get_has_a_called]);
  assert (not [%contract_get_has_b_called])

let () =
  [%contract_main_states]
    (clear ();
     protocol (fun () -> a (); b ());
     clear ();
     (* A second run starts from fresh ghost state. *)
     protocol (fun () -> a (); b ());
     clear ();
     (* Incomplete normal runs are rejected and still reset the state. *)
     expect_blame (fun () -> protocol (fun () -> a ()));
     clear ();
     expect_blame (fun () -> protocol (fun () -> ()));
     clear ();
     (* Catch violations inside the body so the final check can reset state. *)
     expect_blame (fun () -> protocol (fun () -> b ()));
     clear ();
     protocol (fun () ->
       a (); b ();
       expect_blame (fun () -> a ()));
     clear ();
     (* The stated predicates permit repeated A before B and repeated B after A. *)
     protocol (fun () -> a (); a (); b (); b ());
     clear ();
     print_endline "ghost-state protocol: all assertions passed")

(* State is shared by this handler, not private to each protocol instance.
   Nested or concurrent protocols interfere. If the body raises or diverges,
   the range contract does not run, so exceptional-exit cleanup is not provided.
   The state operations and check policy must be trusted. *)
