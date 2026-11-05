(* Handler-placement example.

   A function contract is latent: creating [protected] performs no check.
   The checks happen only when [protected] is applied. *)

let positive x = x > 0

let protected =
  [%mon (positive @-> positive) ~pos:"provider" ~neg:"client" ~loc:"positive -> positive"]
    (fun x -> x)

let show_blame thunk =
  try Printf.printf "result: %d\n" (thunk ()) with
  | Conteff_lib.Contract.Blame { party; contract } ->
      Printf.printf "blame: party=%s, contract=%s\n" party contract

let%contract_main () =

  (* This handler only covers wrapper creation. No check occurs yet, so the
     returned function is not protected by this handler after it escapes. *)
  let escaped =
    Conteff_lib.Contract.with_policy Conteff_lib.Contract.unchecked (fun () -> protected)
  in
  Printf.printf "handler around creation: ";
  show_blame (fun () -> escaped (-1));

  (* Put the handler around the elimination/use of the protected value. Both
     the domain and range checks now occur in its dynamic extent. *)
  Printf.printf "handler around application: ";
  show_blame (fun () ->
      Conteff_lib.Contract.with_policy Conteff_lib.Contract.unchecked
        (fun () -> protected (-1)));

  (* [run] is the implementation counterpart of the formal compiler's
     [wrap]. It enforces checks and recursively handles checks in predicates. *)
  Printf.printf "default run around application: ";
  show_blame (fun () -> Conteff_lib.Contract.run (fun () -> protected (-1)))
