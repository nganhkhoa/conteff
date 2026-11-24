(* Stateful capability building block, corresponding to the Must_call Redex
   example in rewrite/redex/capabilities.

   OCaml 5 and the Conteff PPX are required. The state
   handler surrounds applications as well as the checks they trigger. The
   macro returns only the body result, so tests read the final state inside it.

   All state and instrumentation must be trusted/private. Untrusted state
   operations, bypassing a wrapper/checkpoint, or a custom policy intercepting
   Check can defeat these protocols. The flag is shared within the handler, not per invocation. None is a security theorem. *)
open Conteff_lib

let expect_blame expected_party expected_contract thunk =
  match thunk () with
  | _ -> failwith "expected contract blame"
  | exception Contract.Blame { party; contract } ->
      assert (party = expected_party);
      assert (contract = expected_contract)

[%%contract_states
let has_called : bool = false]

let run thunk =
  [%contract_main_states]
    (let result = thunk () in
     (result, [%contract_get_has_called]))

module Must_call = struct
  let record_call _argument =
    [%contract_set_has_called true];
    true

  let called _result =
    let was_called = [%contract_get_has_called] in
    [%contract_set_has_called false];
    was_called

  let[@contract record_call @-> Contract.any] callback x =
    x + 1

  (* The range checks and resets the obligation on normal return. It records
     attempted entry, not successful return; exceptions skip the range. *)
  let[@contract Contract.any @-> called] program body =
    body callback

  let test () =
    assert (run (fun () ->
        expect_blame "Must_call.Must_call.program" "Must_call.Must_call.program"
          (fun () -> program (fun _ -> 7))) = ((), false));
    assert (run (fun () -> program (fun cb -> cb 4)) = (5, false));
    assert (run (fun () -> program (fun cb -> cb 1 + cb 2)) = (5, false));
    (* A successful call cannot discharge a later invocation’s obligation. *)
    assert (run (fun () ->
        assert (program (fun cb -> cb 4) = 5);
        expect_blame "Must_call.Must_call.program" "Must_call.Must_call.program"
          (fun () -> program (fun _ -> 7))) = ((), false));
    (* A forged write illustrates why state must be private. *)
    assert (run (fun () ->
        program (fun _ -> [%contract_set_has_called true]; 7)) = (7, false))
end


let () =
  Must_call.test ();
  print_endline "Must_call stateful capability example passed."
