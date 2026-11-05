(* The runtime alone: the Check effect, the global handler, and policies. *)

open Conteff_lib
open Helpers

let count = ref 0
let counted p x = incr count; p x
let chk ?(pos = "k") ?(loc = "j") p v = Contract.check ~pos ~loc p v

let pass_under_run () = check_int 5 (fun () -> chk is_pos 5)
let fail_under_run () = check_blame ~party:"k" ~contract:"j" (fun () -> chk is_pos 0)

let unhandled_without_handler () =
  count := 0;
  match chk (counted is_pos) 5 with
  | _ -> Alcotest.fail "expected unhandled Check"
  | exception Effect.Unhandled (Contract.Check _) ->
      Alcotest.(check int) "predicate not evaluated" 0 !count

let predicate_runs_once () =
  count := 0;
  ignore (Contract.run (fun () -> chk (counted is_pos) 1));
  Alcotest.(check int) "calls" 1 !count

let unchecked_skips_predicate () =
  count := 0;
  let v = Contract.with_policy Contract.unchecked (fun () -> chk (counted no) 7) in
  Alcotest.(check int) "value" 7 v;
  Alcotest.(check int) "calls" 0 !count

let enforce_policy_is_default () =
  check_party "k" (fun () -> Contract.with_policy Contract.enforce (fun () -> chk no 1))

let nearest_handler_wins () =
  check_int 1 (fun () -> Contract.with_policy Contract.unchecked (fun () -> chk no 1))

let warn_continues () =
  let seen = ref [] in
  let v =
    Contract.with_policy (Contract.warn (fun b -> seen := b :: !seen))
      (fun () ->
        let a = chk ~pos:"a" no 1 in
        let b = chk ~pos:"b" yes 2 in
        a + b + chk ~pos:"c" no 3)
  in
  Alcotest.(check int) "value" 6 v;
  Alcotest.(check (list string)) "warned" [ "c"; "a" ]
    (List.map (fun (b : Contract.blame) -> b.party) !seen)

let observe_counts () =
  let n = ref 0 in
  let v =
    Contract.with_policy
      (Contract.observe (fun ~pos:_ ~loc:_ -> incr n) Contract.enforce)
      (fun () -> chk yes 1 + chk yes 2)
  in
  Alcotest.(check int) "value" 3 v;
  Alcotest.(check int) "checks" 2 !n

let reassign_party () =
  check_blame ~party:"me" ~contract:"j" (fun () ->
      Contract.with_policy (Contract.reassign "me" Contract.enforce) (fun () -> chk no 1))

let delegate_to_outer () =
  (* the inner handler forwards, the outer one decides *)
  check_int 1 (fun () ->
      Contract.with_policy Contract.unchecked (fun () ->
          Contract.with_policy Contract.delegate (fun () -> chk no 1)))

let delegate_to_default () =
  check_party "k" (fun () -> Contract.with_policy Contract.delegate (fun () -> chk no 1))

let blame_is_catchable_at_monitor () =
  (* blame is raised at the monitor, so code around it can catch it *)
  check_int (-1) (fun () -> try chk no 1 with Contract.Blame _ -> -1)

let predicate_contracts_enforced () =
  (* a check inside a predicate runs under a fresh default handler *)
  let p x = chk ~pos:"inside" yes x > 0 && chk ~pos:"inner" no true in
  check_party "inner" (fun () -> chk p 1)

let printer () =
  Alcotest.(check string) "printer" "Blame(party=\"a\", contract=\"b\")"
    (Printexc.to_string (Contract.Blame { party = "a"; contract = "b" }))

let () =
  Alcotest.run "runtime"
    [ ( "default handler",
        [ case "pass under run" pass_under_run;
          case "fail under run" fail_under_run;
          case "unhandled without handler" unhandled_without_handler;
          case "predicate runs once" predicate_runs_once;
          case "blame catchable at monitor" blame_is_catchable_at_monitor;
          case "contracts inside predicates" predicate_contracts_enforced;
          case "printer" printer ] );
      ( "policies",
        [ case "unchecked skips predicate" unchecked_skips_predicate;
          case "enforce" enforce_policy_is_default;
          case "nearest handler wins" nearest_handler_wins;
          case "warn continues" warn_continues;
          case "observe counts" observe_counts;
          case "reassign party" reassign_party;
          case "delegate to outer" delegate_to_outer;
          case "delegate to default" delegate_to_default ] ) ]
