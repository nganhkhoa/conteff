(* With OCaml 5 and Conteff PPX preprocessing enabled in this directory,
   run from the impl root: dune exec examples/capabilities/exceptions.exe

   These are boundary examples, not a capability-security implementation.
   The runtime selects explicitly enumerated typed constructors, whereas the
   Redex examples instrument all operation names known to their compiler. A
   nearer handler may consume an operation, native mutation/exceptions are not
   intercepted, and a custom Check policy can bypass checks. Unlike that Redex
   model, the runtime blames the handler for a bad response. *)
open Conteff_lib

type _ Effect.t +=
  | Raise_exn : int -> int Effect.t
  | Touch : int -> int Effect.t
  | Emit : int -> int Effect.t
  | Service : int -> int Effect.t

let perform_raise n = Effect.perform (Raise_exn n)

(* The PPX requires one attribute per selected constructor.  The returned
   function monitors each thunk application, not arbitrary OCaml operations. *)
let boundary ~(request : int -> bool) ~(response : int -> bool) =
  let[@effect_contract Raise_exn (request |> response)]
      [@effect_contract Touch (request |> response)]
      [@effect_contract Emit (request |> response)]
      [@effect_contract Service (request |> response)] monitored f = f () in
  monitored

let yes _ = true
let no _ = false

(* Trusted outer interpreter; for Raise_exn, aborting drops the continuation,
   so the forwarded operation never returns and its response is not checked. *)
let handle ?(abort = false) ?(answer = (Fun.id : int -> int)) (f : unit -> int) : int =
  Effect.Deep.match_with f ()
    { Effect.Deep.retc = Fun.id;
      exnc = raise;
      Effect.Deep.effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Raise_exn n ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              if abort then 99 else Effect.Deep.continue k (answer n))
        | _ -> None) }

let expect_blame party loc f =
  match f () with
  | _ -> failwith ("expected blame: " ^ party ^ "/" ^ loc)
  | exception Contract.Blame { party = p; contract = l } ->
      assert (p = party && l = loc)

let exceptions () =
  let rejected = boundary ~request:no ~response:yes in
  expect_blame "Exceptions.boundary.monitored" "Exceptions.boundary.monitored" (fun () ->
    handle ~abort:true (fun () -> rejected (fun () -> perform_raise 7)));
  let response_called = ref false in
  let allowed = boundary ~request:yes
      ~response:(fun _ -> response_called := true; false) in
  assert (handle ~abort:true (fun () -> allowed (fun () -> perform_raise 7)) = 99);
  assert (not !response_called);
  (* An internal handler hides even a forbidden algebraic raise. *)
  assert (rejected (fun () -> handle ~abort:true (fun () -> perform_raise 7)) = 99)


let%contract_main () =
  exceptions ();
  print_endline "exceptions: all assertions passed"
