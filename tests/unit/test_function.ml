(* Function contracts: C-Mon-Func. Most cases are ported from the
   check-semantics tests in redex/compiler.rkt. Labels: k = server
   (positive), l = client (negative), j = contract. *)

open Conteff_lib
open Helpers

let succ x = x + 1

let pass () =
  check_int 11 (fun () -> ([%mon (yes @-> yes) ~pos:"k" ~neg:"l" ~loc:"j"] succ) 10)

let range_blames_server () =
  check_blame ~party:"k" ~contract:"j" (fun () ->
      ([%mon (yes @-> no) ~pos:"k" ~neg:"l" ~loc:"j"] succ) 10)

let domain_blames_client () =
  check_blame ~party:"l" ~contract:"j" (fun () ->
      ([%mon (is_pos @-> yes) ~pos:"k" ~neg:"l" ~loc:"j"] succ) (-5))

let wrapping_does_not_check () =
  let n = ref 0 in
  let policy = Contract.observe (fun ~pos:_ ~loc:_ -> incr n) Contract.enforce in
  let _wrapped = Contract.with_policy policy (fun () -> [%mon no @-> no] succ) in
  Alcotest.(check int) "checks" 0 !n

let higher_order_pass () =
  check_int 6 (fun () ->
      ([%mon ((is_pos @-> yes) @-> yes) ~pos:"k" ~neg:"l"] (fun g -> g 3)) (fun x -> x * 2))

let higher_order_client_function_bad () =
  (* the client's g breaks its range: the client is blamed *)
  check_party "l" (fun () ->
      ([%mon ((yes @-> is_pos) @-> yes) ~pos:"k" ~neg:"l"] (fun g -> g 3)) (fun x -> 0 - x))

let higher_order_server_misuses () =
  (* the server calls g with a bad argument: the server is blamed *)
  check_party "k" (fun () ->
      ([%mon ((is_pos @-> yes) @-> yes) ~pos:"k" ~neg:"l"] (fun g -> g (-3))) (fun x -> x))

let curried_pass () =
  check_int 3 (fun () -> ([%mon yes @-> yes @-> gt 0] (fun a b -> a + b)) 1 2)

let curried_fail () =
  check_party "k" (fun () -> ([%mon (yes @-> yes @-> gt 0) ~pos:"k"] (fun a b -> a + b)) 1 (-5))

let curried_partial_no_check () =
  let n = ref 0 in
  let policy = Contract.observe (fun ~pos:_ ~loc:_ -> incr n) Contract.enforce in
  let _partial : int -> int =
    Contract.with_policy policy (fun () -> ([%mon yes @-> yes @-> yes] (fun a b -> a + b)) 1)
  in
  Alcotest.(check int) "only the first argument" 1 !n

let nested_monitors () =
  check_int 50 (fun () ->
      ([%mon yes @-> lt 100] ([%mon is_pos @-> yes] (fun x -> x * 10))) 5)

let nested_inner_domain () =
  (* the outer contract passes -1 along; the inner contract blames its client *)
  check_party "outer" (fun () ->
      ([%mon (yes @-> lt 100) ~pos:"outer" ~neg:"main"]
         ([%mon (is_pos @-> yes) ~pos:"inner" ~neg:"outer"] (fun x -> x * 10)))
        (-1))

let nested_outer_range () =
  check_party "outer" (fun () ->
      ([%mon (yes @-> lt 100) ~pos:"outer" ~neg:"main"]
         ([%mon (is_pos @-> yes) ~pos:"inner" ~neg:"outer"] (fun x -> x * 10)))
        20)

let every_call_checked () =
  let f = [%mon (is_pos @-> yes) ~neg:"l"] succ in
  check_int 2 (fun () -> f 1);
  check_party "l" (fun () -> f 0);
  check_int 3 (fun () -> f 2)

let two_checks_per_call () =
  let n = ref 0 in
  let policy = Contract.observe (fun ~pos:_ ~loc:_ -> incr n) Contract.enforce in
  let f = [%mon yes @-> yes] succ in
  ignore (Contract.with_policy policy (fun () -> f 1 + f 2));
  Alcotest.(check int) "checks" 4 !n

let domain_checked_before_body () =
  let log = ref [] in
  let f = [%mon (fun _ -> log := "dom" :: !log; true) @-> (fun _ -> log := "rng" :: !log; true)]
      (fun x -> log := "body" :: !log; x)
  in
  ignore (Contract.run (fun () -> f 1));
  Alcotest.(check (list string)) "order" [ "dom"; "body"; "rng" ] (List.rev !log)

let contract_evaluated_once () =
  let made = ref 0 in
  let f = [%mon (incr made; is_pos) @-> yes] succ in
  ignore (Contract.run (fun () -> f 1 + f 2 + f 3));
  Alcotest.(check int) "contract expression evaluated" 1 !made

let polymorphic_wrapped () =
  let id = [%mon yes @-> yes] (fun x -> x) in
  check_int 1 (fun () -> id 1)

let () =
  Alcotest.run "function"
    [ ( "function",
        [ case "pass" pass;
          case "range blames server" range_blames_server;
          case "domain blames client" domain_blames_client;
          case "wrapping does not check" wrapping_does_not_check;
          case "higher-order pass" higher_order_pass;
          case "higher-order client function bad" higher_order_client_function_bad;
          case "higher-order server misuses" higher_order_server_misuses;
          case "curried pass" curried_pass;
          case "curried fail" curried_fail;
          case "curried partial application" curried_partial_no_check;
          case "nested monitors" nested_monitors;
          case "nested inner domain" nested_inner_domain;
          case "nested outer range" nested_outer_range;
          case "every call checked" every_call_checked;
          case "two checks per call" two_checks_per_call;
          case "domain before body" domain_checked_before_body;
          case "contract evaluated once" contract_evaluated_once;
          case "polymorphic" polymorphic_wrapped ] ) ]
