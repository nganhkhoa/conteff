(* With OCaml 5 and Conteff PPX preprocessing enabled in this directory,
   run from the impl root: dune exec examples/capabilities/restricted_effect.exe

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

let emit n = Effect.perform (Emit n)
let service n = Effect.perform (Service n)

(* The PPX requires one attribute per selected constructor.  The returned
   function monitors each thunk application, not arbitrary OCaml operations. *)
let boundary ~(request : int -> bool) ~(response : int -> bool) =
  let[@effect_contract Raise_exn (request |> response)]
      [@effect_contract Touch (request |> response)]
      [@effect_contract Emit (request |> response)]
      [@effect_contract Service (request |> response)] monitored f = f () in
  monitored

let yes _ = true
let positive n = n > 0

(* Trusted outer interpreter for the operations used below. *)
let handle ?(answer = (Fun.id : int -> int)) (f : unit -> int) : int =
  Effect.Deep.match_with f ()
    { Effect.Deep.retc = Fun.id;
      exnc = raise;
      Effect.Deep.effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Emit n ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (answer n))
        | Service n ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k (answer n))
        | _ -> None) }

let expect_blame party loc f =
  match f () with
  | _ -> failwith ("expected blame: " ^ party ^ "/" ^ loc)
  | exception Contract.Blame { party = p; contract = l } ->
      assert (p = party && l = loc)

let restricted_effect () =
  let c = boundary ~request:positive ~response:positive in
  assert (handle (fun () -> c (fun () -> service 3)) = 3);
  expect_blame "Restricted_effect.boundary.monitored" "Restricted_effect.boundary.monitored" (fun () ->
    handle (fun () -> c (fun () -> service (-1))));
  expect_blame "Restricted_effect.boundary" "Restricted_effect.boundary.monitored" (fun () ->
    handle ~answer:(fun _ -> -1) (fun () -> c (fun () -> service 3)));
  let c = boundary ~request:positive ~response:yes in
  expect_blame "Restricted_effect.boundary.monitored" "Restricted_effect.boundary.monitored" (fun () ->
    handle (fun () -> c (fun () ->
      let x = service 2 in let y = service (-1) in x + y)));
  (* The same payload policy applies to every constructor selected above. *)
  expect_blame "Restricted_effect.boundary.monitored" "Restricted_effect.boundary.monitored" (fun () ->
    handle (fun () -> c (fun () -> emit (-1))))


let%contract_main () =
  restricted_effect ();
  print_endline "restricted_effect: all assertions passed"
