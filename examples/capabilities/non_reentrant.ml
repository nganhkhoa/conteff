(* Stateful capability building block, corresponding to the Non_reentrant Redex
   example in rewrite/redex/capabilities.

   OCaml 5 and the Conteff PPX are required. The state
   handler surrounds applications as well as the checks they trigger. The
   macro returns only the body result, so tests read the final state inside it.

   All state and instrumentation must be trusted/private. Untrusted state
   operations, bypassing a wrapper/checkpoint, or a custom policy intercepting
   Check can defeat these protocols. Each example uses one shared integer per
   execution, not per object or invocation. None is a security theorem. *)
open Conteff_lib

let expect_blame expected_party expected_contract thunk =
  match thunk () with
  | _ -> failwith "expected contract blame"
  | exception Contract.Blame { party; contract } ->
      assert (party = expected_party);
      assert (contract = expected_contract)

let run initial thunk =
  [%contract_main_state initial]
    (let result = thunk () in
     (result, Contract.get_state ()))

module Non_reentrant = struct
  let enter _ =
    if Contract.get_state () = 0 then (Contract.set_state 1; true)
    else false

  (* The result predicate runs after a normal return. It ignores the returned
     value, clears the busy bit, and accepts the result. *)
  let leave _result =
    Contract.set_state 0;
    true

  (* Entry instrumentation and the range check must both be used. Clearing
     happens on normal return only, not on blame, exceptions, or divergence. *)
  let[@contract enter @-> leave] f callback =
    callback 1

  let test () =
    assert (run 0 (fun () -> f (fun x -> x + 2)) = (3, 0));
    assert (run 0 (fun () -> f Fun.id + f (fun x -> x + 1)) = (3, 0));
    assert (run 0 (fun () ->
        expect_blame "Non_reentrant.Non_reentrant.test" "Non_reentrant.Non_reentrant.f"
          (fun () -> f (fun _ -> f Fun.id))) = ((), 1));
    assert (run 1 (fun () ->
        expect_blame "Non_reentrant.Non_reentrant.test" "Non_reentrant.Non_reentrant.f" (fun () -> f Fun.id)) = ((), 1))
end


let () =
  Non_reentrant.test ();
  print_endline "Non_reentrant stateful capability example passed."
