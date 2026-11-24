(* Stateful capability building block, corresponding to the Allow_call Redex
   example in rewrite/redex/capabilities.

   OCaml 5 and the Conteff PPX are required. The state
   handler surrounds applications as well as the checks they trigger. The
   macro returns only the body result, so tests read the final state inside it.

   All state and instrumentation must be trusted/private. Untrusted state
   operations, bypassing a wrapper/checkpoint, or a custom policy intercepting
   Check can defeat these protocols. The permission state is shared within each handler, not per invocation. None is a security theorem. *)
open Conteff_lib

let expect_blame expected_party expected_contract thunk =
  match thunk () with
  | _ -> failwith "expected contract blame"
  | exception Contract.Blame { party; contract } ->
      assert (party = expected_party);
      assert (contract = expected_contract)

[%%contract_states
let allowed_to_call : bool = false]

let run thunk =
  [%contract_main_states]
    (let result = thunk () in
     (result, [%contract_get_allowed_to_call]))

module Allow_call = struct
  let permitted _argument = [%contract_get_allowed_to_call]

  let[@contract permitted @-> Contract.any] callback x =
    x + 1

  let grant_permission _body =
    [%contract_set_allowed_to_call true];
    true

  let revoke_permission _result =
    [%contract_set_allowed_to_call false];
    true

  (* The domain grants permission and the range revokes it on normal return.
     An exception skips revocation, and nested extents share the same bit. *)
  let[@contract grant_permission @-> revoke_permission] allowed f = f ()

  let test () =
    assert (run (fun () ->
        expect_blame "Allow_call.Allow_call.test" "Allow_call.Allow_call.callback" (fun () -> callback 4)) = ((), false));
    assert (run (fun () -> allowed (fun () -> callback 4)) = (5, false));
    assert (run (fun () -> allowed (fun () -> callback 1 + callback 2)) = (5, false));
    assert (run (fun () ->
        ignore (allowed (fun () -> callback 4));
        expect_blame "Allow_call.Allow_call.test" "Allow_call.Allow_call.callback" (fun () -> callback 9)) = ((), false));
    (* An abort leaves permission set; this is not general dynamic extent. *)
    assert (run (fun () ->
        (try allowed (fun () -> raise Exit) with Exit -> ());
        callback 4) = (5, true))
end


let () =
  Allow_call.test ();
  print_endline "Allow_call stateful capability example passed."
