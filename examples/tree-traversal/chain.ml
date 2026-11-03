open Effect
open Effect.Deep

type _ Effect.t += Check : int -> bool Effect.t

let build_handler_stack ~mode (preds : (int -> bool) list) =
  match mode with
  | `And ->
      (* 1. The Base Handler: This will end up wrapping the entire execution stack *)
      let base_handler action =
        match_with action () {
          retc = (fun res -> res);
          exnc = (fun e -> raise e);
          effc = (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Check _v -> Some (fun _k -> true) (* Topmost boundary: total success *)
            | _ -> None
          )
        }
      in
      (* 2. Fold Right: acc wraps the predicate handler.
         This produces: base_handler (fun () -> p2_handler (fun () -> p1_handler action)) *)
      List.fold_right (fun p acc action ->
        acc (fun () ->
          match_with action () {
            retc = (fun res -> res);
            exnc = (fun e -> raise e);
            effc = (fun (type c) (eff : c Effect.t) ->
              match eff with
              | Check v -> Some (fun _k ->
                  if p v then perform (Check v) else false
                )
              | _ -> None
            )
          }
        )
      ) preds base_handler

  | `Or ->
      let base_handler action =
        match_with action () {
          retc = (fun res -> res);
          exnc = (fun e -> raise e);
          effc = (fun (type c) (eff : c Effect.t) ->
            match eff with
            | Check _v -> Some (fun _k -> false) (* Topmost boundary: total failure *)
            | _ -> None
          )
        }
      in
      List.fold_right (fun p acc action ->
        acc (fun () ->
          match_with action () {
            retc = (fun res -> res);
            exnc = (fun e -> raise e);
            effc = (fun (type c) (eff : c Effect.t) ->
              match eff with
              | Check v -> Some (fun _k ->
                  if p v then true else perform (Check v)
                )
              | _ -> None
            )
          }
        )
      ) preds base_handler

let evaluate_predicates ~mode preds x =
  let handler_tower = build_handler_stack ~mode preds in
  (* Pass the initial catalyst to the innermost layer of the generated tower *)
  handler_tower (fun () -> perform (Check x))

let () =
  let is_even v =
    Printf.printf "check is_even\n";
    v mod 2 = 0 in
  let is_positive v =
    Printf.printf "check is_positive\n";
    v > 0 in
  let is_large v =
    Printf.printf "check is_large\n";
    v > 100 in

  let pred_list = [is_even; is_positive; is_large] in

  (* --- AND Validation --- *)
  let and_true = evaluate_predicates ~mode:`And pred_list 102 in
  Printf.printf "[AND] 102 expected true:  %b\n" and_true;

  let and_false = evaluate_predicates ~mode:`And pred_list 9 in
  Printf.printf "[AND] 9   expected false: %b\n" and_false;

  (* --- OR Validation --- *)
  let or_true = evaluate_predicates ~mode:`Or pred_list (-4) in
  Printf.printf "[OR]  -4  expected true:  %b\n" or_true;

  let or_false = evaluate_predicates ~mode:`Or pred_list (-5) in
  Printf.printf "[OR]  -5  expected false: %b\n" or_false;
  ()

