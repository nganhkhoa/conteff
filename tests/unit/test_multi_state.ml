open Conteff_lib
open Helpers

let int_initializations = ref 0
let string_initializations = ref 0

[%%contract_states
  let count : int = (incr int_initializations; 10)
  and label : string = (incr string_initializations; "initial")]

let check p x = Contract.check ~pos:"server" ~loc:"multi" p x

let heterogeneous_states () =
  let result = [%contract_main_states]
    (let before = ([%contract_get_count], [%contract_get_label]) in
     [%contract_set_count ([%contract_get_count] + 5)];
     let after_count = ([%contract_get_count], [%contract_get_label]) in
     [%contract_set_label ([%contract_get_label] ^ "!")];
     (before, after_count, ([%contract_get_count], [%contract_get_label]))) in
  Alcotest.(check (triple (pair int string) (pair int string) (pair int string)))
    "independent heterogeneous slots" ((10, "initial"), (15, "initial"), (15, "initial!")) result

let nested_predicates () =
  let inner x =
    [%contract_set_count ([%contract_get_count] + 2)];
    [%contract_set_label "inner"];
    x > 0 in
  let outer x =
    [%contract_set_count ([%contract_get_count] + 1)];
    ignore (check inner x);
    let seen = ([%contract_get_count], [%contract_get_label]) in
    [%contract_set_count ([%contract_get_count] + 3)];
    [%contract_set_label ([%contract_get_label] ^ "/outer")];
    seen = (13, "inner") in
  let result = [%contract_main_states]
    (let value = check outer 4 in
     (value, [%contract_get_count], [%contract_get_label])) in
  Alcotest.(check (triple int int string)) "nested updates reach continuation"
    (4, 16, "inner/outer") result

let caught_blame () =
  let fail _ =
    [%contract_set_count 42];
    [%contract_set_label "failed"];
    false in
  let result = [%contract_main_states]
    (try ignore (check fail 1); Alcotest.fail "expected blame"
     with Contract.Blame b ->
       let state = ([%contract_get_count], [%contract_get_label]) in
       [%contract_set_count 43];
       (b, state, [%contract_get_count])) in
  let b, state, after = result in
  Alcotest.check blame "blame preserved" { Contract.party = "server"; contract = "multi" } b;
  Alcotest.(check (pair int string)) "updates survive failure" (42, "failed") state;
  Alcotest.(check int) "handler remains active after catch" 43 after

exception Predicate_failure

let caught_predicate_exception () =
  let fail _ =
    [%contract_set_count 99];
    [%contract_set_label "exception"];
    raise Predicate_failure in
  let result = [%contract_main_states]
    (try ignore (check fail 1); Alcotest.fail "expected predicate exception"
     with Predicate_failure -> ([%contract_get_count], [%contract_get_label])) in
  Alcotest.(check (pair int string)) "updates survive predicate exception" (99, "exception") result

let fresh_runs () =
  int_initializations := 0;
  string_initializations := 0;
  let run () = [%contract_main_states]
    (let before = ([%contract_get_count], [%contract_get_label]) in
     [%contract_set_count 77];
     [%contract_set_label "changed"];
     before) in
  Alcotest.(check (pair int string)) "first initial state" (10, "initial") (run ());
  Alcotest.(check (pair int int)) "each initializer once" (1, 1)
    (!int_initializations, !string_initializations);
  Alcotest.(check (pair int string)) "second initial state" (10, "initial") (run ());
  Alcotest.(check (pair int int)) "each initializer once per run" (2, 2)
    (!int_initializations, !string_initializations)

let monitored_function () =
  let domain x =
    [%contract_set_count ([%contract_get_count] + 1)];
    [%contract_set_label ([%contract_get_label] ^ ".")];
    x > 0 in
  let f = [%mon (domain @-> Contract.any) ~neg:"caller" ~loc:"argument"] (fun x -> x + 1) in
  let result = [%contract_main_states]
    (let a = f 1 in
     let b = f 2 in
     (a, b, ([%contract_get_count], [%contract_get_label]))) in
  Alcotest.(check (triple int int (pair int string))) "state shared across calls"
    (2, 3, (12, "initial..")) result

let () = Alcotest.run "multi state"
  [ "state", [case "heterogeneous slots" heterogeneous_states;
              case "nested predicates" nested_predicates;
              case "caught blame" caught_blame;
              case "caught predicate exception" caught_predicate_exception;
              case "fresh runs and initialization" fresh_runs;
              case "monitored function calls" monitored_function] ]
