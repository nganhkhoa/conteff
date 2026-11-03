(* Custom handlers around compiled checks: the redex/compiler3.rkt
   experiment, plus policies. *)

open Conteff_lib
open Helpers

let succ x = x + 1

(* the compiler3.rkt table *)
let one_hijacked () = check_int 3 (fun () -> [%hijack] ([%mon no] 1) + [%mon yes] 2)

let source_semantics_blames () = check_party "k" (fun () -> [%mon no ~pos:"k"] 1 + [%mon yes] 2)

let other_mon_still_checked () =
  check_party "other" (fun () -> [%hijack] ([%mon yes] 1) + [%mon no ~pos:"other"] 2)

let both_fail_one_hijacked () =
  check_party "other" (fun () -> [%hijack] ([%mon no] 1) + [%mon no ~pos:"other"] 2)

let dynamic_extent () = check_int 3 (fun () -> [%hijack] ([%mon no] 1 + [%mon no] 2))

let tuple_hijacked () = check_pair (1, 2) (fun () -> [%hijack] ([%mon (no, no)] (1, 2)))

let application_hijacked () = check_int 11 (fun () -> [%hijack] (([%mon yes @-> no] succ) 10))

let wrapper_escapes () =
  (* only creating the wrapper is hijacked; its checks run later *)
  check_party "k" (fun () -> ([%hijack] ([%mon (yes @-> no) ~pos:"k"] succ)) 10)

let dependent_hijacked () =
  check_int 11 (fun () -> [%hijack] (([%mon dep yes (fun y -> fun r -> r - y = 0)] succ) 10))

let closure_escapes () =
  check_party "k" (fun () ->
      let g = [%hijack] (fun () -> [%mon no ~pos:"k"] 1) in
      g ())

(* policies *)
let enforce_policy () = check_party "k" (fun () -> [%hijack Contract.enforce] ([%mon no ~pos:"k"] 1))

let warn_policy () =
  let seen = ref [] in
  let v =
    Contract.run (fun () ->
        [%hijack Contract.warn (fun b -> seen := b.party :: !seen)]
          (let a = [%mon no ~pos:"a"] 1 in
           a + ([%mon (yes @-> no) ~pos:"b"] succ) 1))
  in
  Alcotest.(check int) "value" 3 v;
  Alcotest.(check (list string)) "warnings" [ "a"; "b" ] (List.rev !seen)

let count_checks () =
  let n = ref 0 in
  let counting = Contract.observe (fun ~pos:_ ~loc:_ -> incr n) Contract.enforce in
  let f = [%mon yes @-> yes] succ in
  check_int 5 (fun () -> [%hijack counting] (f (f 1) + [%mon yes] 2));
  Alcotest.(check int) "checks" 5 !n

let reassign_policy () =
  check_blame ~party:"me" ~contract:"j" (fun () ->
      [%hijack Contract.reassign "me" Contract.enforce] ([%mon no ~pos:"k" ~loc:"j"] 1))

let nested_inner_wins () =
  check_party "k" (fun () -> [%hijack] ([%hijack Contract.enforce] ([%mon no ~pos:"k"] 1)))

let nested_outer_after_inner () =
  (* the inner hijack ends; the next check is the outer hijack's *)
  check_int 2 (fun () -> [%hijack] ([%hijack Contract.enforce] ([%mon yes] 1) + [%mon no] 1))

let delegate_logs () =
  let log = ref [] in
  let logging = Contract.observe (fun ~pos ~loc:_ -> log := pos :: !log) Contract.delegate in
  check_party "b" (fun () -> [%hijack logging] (let a = [%mon yes ~pos:"a"] 1 in a + [%mon no ~pos:"b"] 2));
  Alcotest.(check (list string)) "logged" [ "a"; "b" ] (List.rev !log)

let delegate_to_hijack () =
  check_int 3 (fun () -> [%hijack] ([%hijack Contract.delegate] ([%mon no] 1 + [%mon no] 2)))

let predicates_not_hijacked () =
  (* warn runs the predicate under the default handler, so a check inside
     the predicate is still enforced *)
  let p x = [%mon no ~pos:"inside"] x > 0 in
  check_party "inside" (fun () -> [%hijack Contract.warn ignore] ([%mon p] 1))

let blame_catchable_inside () =
  check_int (-1) (fun () ->
      [%hijack Contract.enforce] (try [%mon no] 1 with Contract.Blame _ -> -1))

let custom_policy () =
  (* only checks from "trusted" are skipped *)
  let trust =
    { Contract.on_check =
        (fun ~pos ~loc pred v ->
          if pos = "trusted" then v else Contract.enforce.on_check ~pos ~loc pred v) }
  in
  check_int 1 (fun () -> [%hijack trust] ([%mon no ~pos:"trusted"] 1));
  check_party "other" (fun () -> [%hijack trust] ([%mon no ~pos:"other"] 1))

let () =
  Alcotest.run "hijack"
    [ ( "compiler3",
        [ case "one hijacked" one_hijacked;
          case "source semantics blames" source_semantics_blames;
          case "other mon still checked" other_mon_still_checked;
          case "both fail, one hijacked" both_fail_one_hijacked;
          case "dynamic extent" dynamic_extent;
          case "tuple" tuple_hijacked;
          case "application" application_hijacked;
          case "wrapper escapes" wrapper_escapes;
          case "dependent" dependent_hijacked;
          case "closure escapes" closure_escapes ] );
      ( "policies",
        [ case "enforce" enforce_policy;
          case "warn" warn_policy;
          case "count checks" count_checks;
          case "reassign" reassign_policy;
          case "nested inner wins" nested_inner_wins;
          case "nested outer after inner" nested_outer_after_inner;
          case "delegate logs" delegate_logs;
          case "delegate to hijack" delegate_to_hijack;
          case "predicates not hijacked" predicates_not_hijacked;
          case "blame catchable inside" blame_catchable_inside;
          case "custom policy" custom_policy ] ) ]
