(* With OCaml 5 and Conteff PPX preprocessing enabled in this directory,
   run from the impl root: dune exec examples/capabilities/purity.exe

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
  | Other : int -> int Effect.t

let emit n = Effect.perform (Emit n)
let other n = Effect.perform (Other n)

(* The PPX requires one attribute per selected constructor.  The returned
   function monitors each thunk application, not arbitrary OCaml operations. *)
let boundary ~(request : int -> bool) ~(response : int -> bool) =
  let[@effect_contract Raise_exn (request |> response)]
      [@effect_contract Touch (request |> response)]
      [@effect_contract Emit (request |> response)]
      [@effect_contract Service (request |> response)] monitored f = f () in
  monitored

let no _ = false

(* Trusted outer interpreter for the operations used below. *)
let handle (f : unit -> int) : int =
  Effect.Deep.match_with f ()
    { Effect.Deep.retc = Fun.id;
      exnc = raise;
      Effect.Deep.effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Emit n ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k n)
        | Other n ->
            Some (fun (k : (a, _) Effect.Deep.continuation) ->
              Effect.Deep.continue k n)
        | _ -> None) }

let expect_blame party loc f =
  match f () with
  | _ -> failwith ("expected blame: " ^ party ^ "/" ^ loc)
  | exception Contract.Blame { party = p; contract = l } ->
      assert (p = party && l = loc)

let purity () =
  let c = boundary ~request:no ~response:no in
  assert (c (fun () -> 2 + 3) = 5);
  expect_blame "Purity.boundary.monitored" "Purity.boundary.monitored" (fun () ->
    handle (fun () -> c (fun () -> emit 1)));
  assert (c (fun () -> handle (fun () -> emit 1)) = 1);
  let cell = ref 1 in
  assert (c (fun () -> cell := 9; !cell) = 9);
  (* Explicitly unenumerated operations pass through, even under this policy. *)
  assert (handle (fun () -> c (fun () -> other 1)) = 1)


let%contract_main () =
  purity ();
  print_endline "purity: all assertions passed"
