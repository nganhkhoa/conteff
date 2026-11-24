(* Runtime effect contracts are independent of the PPX syntax. *)
open Conteff_lib
open Helpers

type _ Effect.t += Query : int -> int Effect.t | Other : string Effect.t
exception Handler_failure
exception Predicate_failure

let perform_query n = Effect.perform (Query n)

let monitor ?(pos = "producer") ?(neg = "handler") ?(loc = "query/c")
    ~request:(request : int -> bool) ~response:(response : int -> bool) () : Contract.effect_contract =
  let intercept : type a. a Effect.t -> (unit -> a) option = function
    | Query n ->
        Some (fun () ->
            Contract.forward_effect ~pos ~neg ~loc ~request ~response
              perform_query n)
    | _ -> None
  in
  { intercept }

(* A real outside handler: forwarding must reach it, not the monitor again. *)
let handle ~(query : int -> int) thunk =
  Effect.Deep.try_with thunk ()
    { effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Query n ->
              Some (fun (k : (a, _) Effect.Deep.continuation) ->
                  match query n with
                  | value -> Effect.Deep.continue k value
                  | exception e -> Effect.Deep.discontinue k e)
          | Other ->
              Some (fun (k : (a, _) Effect.Deep.continuation) ->
                  Effect.Deep.continue k "outside")
          | _ -> None) }

let pass_and_preserve_values () =
  let seen = ref [] in
  let c = monitor ~request:is_pos ~response:is_even () in
  let result =
    Contract.run (fun () ->
        handle ~query:(fun n -> seen := n :: !seen; n + 1) (fun () ->
            Contract.with_effect_contract c (fun () -> perform_query 3)))
  in
  Alcotest.(check int) "response" 4 result;
  Alcotest.(check (list int)) "forwarded payload" [ 3 ] !seen

let bad_request () =
  let calls = ref 0 in
  let c = monitor ~request:is_pos ~response:yes () in
  check_blame ~party:"producer" ~contract:"query/c" (fun () ->
      handle ~query:(fun n -> incr calls; n) (fun () ->
          Contract.with_effect_contract c (fun () -> perform_query 0)));
  Alcotest.(check int) "handler not called for invalid request" 0 !calls

let bad_response () =
  let calls = ref 0 in
  let c = monitor ~request:is_pos ~response:is_even () in
  check_blame ~party:"handler" ~contract:"query/c" (fun () ->
      handle ~query:(fun _ -> incr calls; 3) (fun () ->
          Contract.with_effect_contract c (fun () -> perform_query 1)));
  Alcotest.(check int) "handler called once" 1 !calls

let repeated_effects () =
  let requests = ref [] and responses = ref [] and handled = ref [] in
  let c =
    monitor
      ~request:(fun n -> requests := n :: !requests; n > 0)
      ~response:(fun n -> responses := n :: !responses; n > 0) ()
  in
  let result =
    Contract.run (fun () ->
        handle ~query:(fun n -> handled := n :: !handled; n * 2) (fun () ->
            Contract.with_effect_contract c (fun () ->
                let a = perform_query 1 in
                let b = perform_query 2 in
                a + b)))
  in
  Alcotest.(check int) "sum" 6 result;
  Alcotest.(check (list int)) "requests" [ 1; 2 ] (List.rev !requests);
  Alcotest.(check (list int)) "handled" [ 1; 2 ] (List.rev !handled);
  Alcotest.(check (list int)) "responses" [ 2; 4 ] (List.rev !responses)

let unmentioned_effect () =
  let c = monitor ~request:yes ~response:yes () in
  let result =
    Contract.run (fun () ->
        handle ~query:Fun.id (fun () ->
            Contract.with_effect_contract c (fun () ->
                let message = Effect.perform Other in
                (message, perform_query 5))))
  in
  Alcotest.(check (pair string int)) "both effects" ("outside", 5) result

let exception_from_body () =
  let c = monitor ~request:yes ~response:yes () in
  Alcotest.check_raises "body exception escapes" Handler_failure (fun () ->
      Contract.run (fun () ->
          Contract.with_effect_contract c (fun () -> raise Handler_failure)))

let exception_from_handler_is_catchable () =
  let c = monitor ~request:yes ~response:yes () in
  let calls = ref 0 in
  let result =
    Contract.run (fun () ->
        handle ~query:(fun n -> incr calls; if n = 1 then raise Handler_failure else n)
          (fun () ->
            Contract.with_effect_contract c (fun () ->
                let first =
                  try perform_query 1 with Handler_failure -> 10
                in
                first + perform_query 2)))
  in
  Alcotest.(check int) "caught at perform and continued" 12 result;
  Alcotest.(check int) "monitor remains usable" 2 !calls

let blame_is_catchable_at_perform () =
  let c = monitor ~request:is_pos ~response:yes () in
  let result =
    Contract.run (fun () ->
        handle ~query:Fun.id (fun () ->
            Contract.with_effect_contract c (fun () ->
                let first =
                  try perform_query 0 with
                  | Contract.Blame { party = "producer"; contract = "query/c" } -> 10
                in
                first + perform_query 2)))
  in
  Alcotest.(check int) "catch and resume under monitor" 12 result

let predicate_exception_is_catchable () =
  let c =
    monitor ~request:(fun n -> if n = 1 then raise Predicate_failure else true)
      ~response:yes ()
  in
  let result =
    Contract.run (fun () ->
        handle ~query:Fun.id (fun () ->
            Contract.with_effect_contract c (fun () ->
                let first = try perform_query 1 with Predicate_failure -> 10 in
                first + perform_query 2)))
  in
  Alcotest.(check int) "predicate exception at perform" 12 result

let nested_monitors () =
  let events = ref [] in
  let record name n = events := name :: !events; n > 0 in
  let inner = monitor ~request:(record "inner request") ~response:(record "inner response") () in
  let outer = monitor ~request:(record "outer request") ~response:(record "outer response") () in
  let result =
    Contract.run (fun () ->
        handle ~query:(fun n -> events := "handler" :: !events; n + 1) (fun () ->
            Contract.with_effect_contract outer (fun () ->
                Contract.with_effect_contract inner (fun () -> perform_query 1))))
  in
  Alcotest.(check int) "nested result" 2 result;
  Alcotest.(check (list string)) "cross boundaries in order"
    [ "inner request"; "outer request"; "handler"; "outer response"; "inner response" ]
    (List.rev !events)

let () =
  Alcotest.run "effect contract"
    [ ("monitor",
       [ case "valid request and response" pass_and_preserve_values;
         case "request blame before forwarding" bad_request;
         case "response blame" bad_response;
         case "repeated effects" repeated_effects;
         case "unmentioned effect forwards" unmentioned_effect;
         case "body exception" exception_from_body;
         case "handler exception" exception_from_handler_is_catchable;
         case "blame is catchable" blame_is_catchable_at_perform;
         case "predicate exception" predicate_exception_is_catchable;
         case "nested monitors" nested_monitors ]) ]
