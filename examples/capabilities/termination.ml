(* Stateful capability building block, corresponding to the Termination Redex
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

module Termination = struct
  (* Trusted finite entry checks of a natural-valued rank. The initial state
     is an exclusive upper bound. This is not a general termination checker:
     it cannot detect uninstrumented loops or divergence between checks, and
     a shared cell conflates nested/independent invocations. *)
  let decreasing_rank rank =
    if rank >= 0 && rank < Contract.get_state () then
      (Contract.set_state rank; true)
    else false

  let[@contract Contract.any @-> decreasing_rank] entry rank = rank

  let entries ranks = List.iter (fun rank -> ignore (entry rank)) ranks; 42

  let test () =
    assert (run 4 (fun () -> entries [3; 2; 0]) = (42, 0));
    List.iter (fun (ranks, final_state) ->
        assert (run 4 (fun () ->
            expect_blame "Termination.Termination.entry"
              "Termination.Termination.entry"
              (fun () -> entries ranks)) = ((), final_state)))
      [([3; 3], 3); ([2; 3], 2); ([-1], 4); ([4], 4)];
    assert (run 4 (fun () -> 42) = (42, 4))
end


let () =
  Termination.test ();
  print_endline "Termination stateful capability example passed."
