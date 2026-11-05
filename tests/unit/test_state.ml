open Conteff_lib
open Helpers

let check p x = Contract.check ~pos:"server" ~loc:"state" p x

let increasing_input input =
  let previous = Contract.get_state () in
  if input < previous then false
  else (Contract.set_state input; true)

let budget input =
  let previous = Contract.get_state () in
  if previous = 2 then true
  else (Contract.set_state (previous + 1); input > 0)

let increasing_calls () =
  let f = [%mon (increasing_input @-> Contract.any)
    ~pos:"server" ~neg:"client" ~loc:"inputs"] (fun x -> x + 100) in
  let result, state = Contract.with_state_result ~initial:0 (fun () ->
    let a = f 2 in
    let b = f 5 in
    (a, b)) in
  Alcotest.(check (pair int int)) "results" (102, 105) result;
  Alcotest.(check int) "last input, not output" 5 state

let equal_calls () =
  let f = [%mon (increasing_input @-> Contract.any)] Fun.id in
  let result = [%contract_main_state 0]
    (let a = f 3 in let b = f 3 in (a, b)) in
  Alcotest.(check (pair int int)) "equal allowed" (3, 3) result

let decreasing_calls () =
  let f = [%mon (increasing_input @-> Contract.any)
    ~pos:"server" ~neg:"client" ~loc:"inputs"] Fun.id in
  Alcotest.check_raises "decrease blames caller"
    (Contract.Blame { party = "client"; contract = "inputs" })
    (fun () -> ignore ([%contract_main_state 0]
      (ignore (f 3); f 2)))

let budget_calls () =
  let f = [%mon (budget @-> budget)] Fun.id in
  let result, state = Contract.with_state_result ~initial:0 (fun () ->
    let a = f 1 in let b = f (-3) in (a, b)) in
  Alcotest.(check (pair int int)) "skip after two checks" (1, -3) result;
  Alcotest.(check int) "budget stops" 2 state

let nested_predicate () =
  let inner x = Contract.set_state 7; x > 0 in
  let outer x =
    ignore (check inner x);
    let observed = Contract.get_state () in
    Contract.set_state (observed + 1);
    observed = 7 in
  let value, state = Contract.with_state_result ~initial:0 (fun () ->
    let value = check outer 4 in
    Alcotest.(check int) "continuation sees outer update" 8 (Contract.get_state ());
    value) in
  Alcotest.(check int) "result" 4 value;
  Alcotest.(check int) "nested state" 8 state

let catch_blame () =
  let result, state = Contract.with_state_result ~initial:0 (fun () ->
    try check (fun _ -> Contract.set_state 9; false) 1
    with Contract.Blame _ -> Contract.get_state ()) in
  Alcotest.(check int) "caught at monitor with latest state" 9 result;
  Alcotest.(check int) "state after failure" 9 state

let isolated_runs () =
  let thunk () = let s = Contract.get_state () in Contract.set_state (s + 1); s in
  Alcotest.(check int) "first" 0 ([%contract_main_state 0] (thunk ()));
  Alcotest.(check int) "fresh invocation" 0 ([%contract_main_state 0] (thunk ()))

let main_state_initial () =
  let evaluations = ref 0 in
  let initial () = incr evaluations; 41 in
  let result = [%contract_main_state (initial ())]
    (let before = Contract.get_state () in
     Contract.set_state (before + 1);
     (before, Contract.get_state ())) in
  Alcotest.(check (pair int int)) "initial state and update" (41, 42) result;
  Alcotest.(check int) "initial expression evaluated once" 1 !evaluations

let increment_calls () =
  let count_call _ =
    Contract.set_state (Contract.get_state () + 1); true in
  let f = [%mon (count_call @-> Contract.any)] (fun x -> x + 1) in
  let result, state = Contract.with_state_result ~initial:0 (fun () ->
    let a = f 10 in let b = f 20 in let c = f 30 in (a, b, c)) in
  Alcotest.(check (triple int int int)) "increment function" (11, 21, 31) result;
  Alcotest.(check int) "one increment per call, domain only" 3 state

let deep_preserved () =
  let module D = Conteff_lib.Contract_deep in
  Alcotest.(check int) "old deep runtime" 3
    (D.run (fun () -> D.check ~pos:"p" ~loc:"j" (fun x -> x > 0) 3))

let () = Alcotest.run "shallow state"
  [ "state", [case "increasing inputs" increasing_calls;
              case "equal inputs" equal_calls;
              case "decreasing inputs" decreasing_calls;
              case "two-check budget" budget_calls;
              case "nested predicate state" nested_predicate;
              case "catch blame with updated state" catch_blame;
              case "independent runs" isolated_runs;
              case "main_state initial expression" main_state_initial;
              case "increment every function call" increment_calls;
              case "deep runtime preserved" deep_preserved] ]
