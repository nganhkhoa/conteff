open Effect
open Effect.Shallow

type _ Effect.t += Action : unit Effect.t

type context = { count: int }

(* Fix: Explicitly annotate the return type as a (unit, context) handler *)
let rec action_handler ctx : (unit, context) handler = {
  retc = (fun _ -> ctx);
  exnc = (fun e -> raise e);

  (* Fix: Explicitly type the return of the effc function *)
  effc = fun (type a) (eff : a Effect.t) : ((a, unit) continuation -> context) option ->
    match eff with
    | Action -> Some (fun k ->
        if ctx.count >= 3 then begin
          print_endline "Condition met. Halting self-emission.";
          continue_with k () (action_handler ctx)
        end else begin
          let new_ctx = { count = ctx.count + 1 } in
          Printf.printf "Inside handler: Emitting action %d\n" new_ctx.count;

          let inner_fiber = fiber (fun () -> perform Action) in
          let updated_ctx = continue_with inner_fiber () (action_handler new_ctx) in

          continue_with k () (action_handler updated_ctx)
        end
      )
    | _ -> None
}

let () =
  let initial_ctx = { count = 0 } in
  let k = fiber (fun () -> perform Action) in

  let final_ctx = continue_with k () (action_handler initial_ctx) in
  Printf.printf "Final context count: %d\n" final_ctx.count
