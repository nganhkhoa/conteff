exception Blame of string

module Contract = struct
type _ Effect.t +=
  | Flat : (string * ('a -> bool) * 'a) -> 'a Effect.t

let rec contract_checking : type a b.
  a Effect.t
  -> ((a, b) Effect.Deep.continuation -> b) option
  = fun eff ->
  let open Effect.Deep in
  match eff with
  | Flat (label, check, arg) ->
      Some (fun k ->
        if check arg
        then continue k arg
        else discontinue k (Blame label))
  | _ -> None
end

let run_with_effects thunk =
  Effect.Deep.match_with thunk ()
  {
    Effect.Deep.retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = Contract.contract_checking;
  }


let x_pred x =
  Printf.printf "x_pred %d\n" x;
  true

let g_p1_pred x =
  Printf.printf "g_p1_pred %d\n" x;
  true

let g_p2_pred x =
  Printf.printf "g_p2_pred %d\n" x;
  true

let check_gr p1 p2 x =
  Printf.printf "check_gr %d\n" x;
  true

let f_p1_pred x =
  Printf.printf "f_p1_pred %d\n" x;
  true

let check_fr_function p1 x =
  (* with dependent value *)
  Printf.printf "check_fr_function %d\n" x;
  true

let check_fr_value x =
  Printf.printf "check_fr_value %d\n" x;
  true

let f pos neg cloc (x : int) (g : int -> int -> int) : (int -> int) =
  let x_wrap pos neg cloc x =
    Effect.perform (Contract.Flat (pos, x_pred, x))
  in
  let x = x_wrap neg pos cloc x in

  let g_wrap pos neg cloc g p1 p2 =
    let p1_wrap pos neg cloc x = Effect.perform (Contract.Flat (pos, g_p1_pred, x)) in
    let p1_dep = p1_wrap neg cloc cloc p1 in
    let p1 = p1_wrap neg pos cloc p1 in

    let p2_wrap pos neg cloc x = Effect.perform (Contract.Flat (pos, g_p2_pred, x)) in
    let p2_dep = p2_wrap neg cloc cloc p2 in
    let p2 = p2_wrap neg pos cloc p2 in

    let r_wrap pos neg cloc x = Effect.perform (Contract.Flat (pos, check_gr p1_dep p2_dep, x)) in
    r_wrap pos neg cloc (g p1 p2)
  in
  let g = g_wrap neg pos cloc g in

  (* depending on what body is, this can be a function
     and we return a function wrapped to invoke effects
     however, this only works if we have the handler outside
    *)
  (* let [@contract : outpred] ret : f_type = body in
  ret pos neg cloc *)

  let body =
    (* returns a value *)
    (* g x x *)

    (* returns a function *)
    (* fun y -> x + y *)
    fun y -> g x y
  in

  (* returns a value example *)
  (* let ret_wrap pos neg cloc body =
    Effect.perform (Contract.Flat (pos, check_fr_value, body))
  in
  let ret = ret_wrap pos neg cloc body in
  ret *)

  (* returns a function example *)
  let ret_wrap pos neg cloc body p1 =
    let p1_wrap pos neg cloc x = Effect.perform (Contract.Flat (pos, f_p1_pred, x)) in
    let p1_dep = p1_wrap neg cloc cloc p1 in
    let p1 = p1_wrap neg pos cloc p1 in

    let r pos neg cloc x = Effect.perform (Contract.Flat (pos, check_fr_function p1_dep, x)) in
    r pos neg cloc (body p1)
  in
  let ret = ret_wrap pos neg cloc body in
  (* this computation is ret p1, and it will be wrapped by the handler *)
  fun p1 -> run_with_effects (fun () -> ret p1)


let f = f "f" "main" "f"

let f = fun p1 p2 -> run_with_effects (fun () -> f p1 p2)

let () =
  let g = fun x y -> x + y in
  let h = f 10 g in
  let _ = h 100 in
  ()
