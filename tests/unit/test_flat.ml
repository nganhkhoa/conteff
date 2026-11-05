(* [%mon] with flat contracts: C-Mon-Flat *)

open Conteff_lib
open Helpers

let pass () = check_int 0 (fun () -> [%mon is_zero ~pos:"k" ~neg:"l" ~loc:"j"] 0)

let fail () =
  check_blame ~party:"k" ~contract:"j" (fun () -> [%mon is_zero ~pos:"k" ~neg:"l" ~loc:"j"] 1)

let in_context () = check_int 4 (fun () -> [%mon yes] 2 + 2)

let in_context_fail () = check_party "k" (fun () -> [%mon no ~pos:"k"] 2 + 2)

let default_labels () =
  let b = blamed (fun () -> [%mon no] 1) in
  Alcotest.(check string) "party" "server" b.party;
  Alcotest.(check bool) "contract location" true
    (String.starts_with ~prefix:"test_flat.ml:" b.contract)

let closure_predicate () =
  check_int 11 (fun () -> [%mon gt 10] 11);
  check_party "k" (fun () -> [%mon gt 10 ~pos:"k"] 10)

let predicate_before_value () =
  let log = ref [] in
  let note s = log := s :: !log in
  ignore (Contract.run (fun () -> [%mon (note "pred"; is_pos)] (note "value"; 1)));
  Alcotest.(check (list string)) "order" [ "pred"; "value" ] (List.rev !log)

let nested_inner_fails () =
  check_party "inner" (fun () -> [%mon gt 0 ~pos:"outer"] ([%mon lt 10 ~pos:"inner"] 20))

let nested_outer_fails () =
  check_party "outer" (fun () -> [%mon gt 100 ~pos:"outer"] ([%mon lt 10 ~pos:"inner"] 5))

let other_types () =
  Alcotest.(check string) "string" "abc"
    (Contract.run (fun () -> [%mon fun s -> String.length s > 2] "abc"));
  check_bool true (fun () -> [%mon fun b -> b] true)

let label_expressions () =
  let who = "dynamic" and where = "here" in
  check_blame ~party:"dynamic" ~contract:"here" (fun () -> [%mon no ~pos:who ~loc:where] 1)

let contract_in_predicate () =
  let p x = [%mon no ~pos:"pred-body"] x > 0 in
  check_party "pred-body" (fun () -> [%mon p ~pos:"k"] 1)

let no_handler_installed () =
  match [%mon is_pos] 3 with
  | _ -> Alcotest.fail "expected unhandled Check"
  | exception Effect.Unhandled (Contract.Check _) -> ()

let () =
  Alcotest.run "flat"
    [ ( "flat",
        [ case "pass" pass;
          case "fail" fail;
          case "in context" in_context;
          case "in context fail" in_context_fail;
          case "default labels" default_labels;
          case "closure predicate" closure_predicate;
          case "predicate before value" predicate_before_value;
          case "nested inner fails" nested_inner_fails;
          case "nested outer fails" nested_outer_fails;
          case "other types" other_types;
          case "label expressions" label_expressions;
          case "contract in predicate" contract_in_predicate;
          case "no handler installed" no_handler_installed ] ) ]
