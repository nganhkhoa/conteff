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

let check_x v =
  Printf.printf "check_x %d\n" v;
  true

let check_g_p1_p1 v =
  Printf.printf "check_g_p1_p1 %d\n" v;
  true

let check_g_p1_ret v =
  Printf.printf "check_g_p1_ret %d\n" v;
  true

let check_g_ret p1 v =
  Printf.printf "check_g_ret %d->%d\n" (p1 10) v;
  true

let check_f_p1 x g v =
  Printf.printf "check_f_p1 %d->%d->%d\n" x
  (g (fun _ -> Printf.printf "call g'p1 in check_f_p1"; 20)) v;
  true

let check_f_ret x g p1 v =
  Printf.printf "check_f_ret %d->%d->%d->%d\n" x
  (g (fun _ -> Printf.printf "call g'p1 in check_f_ret"; 30)) p1 v;
  true

(*
   x : check_x
   g : (check_g_p1_p1 -> check_g_p1_ret) ->d check_g_ret
   ->d (check_f_p1 -> check_f_ret)

   this example illustrates how to group effects together
   we don't use local handler anymore, and therefore
   returning a function loses its context
   but a global handler is able to catch all effects

   --> need extra care when moving the function
   if they are run in a non-handler context, effect is thrown
   therefore all effects must be handled

   another problem arises when we support dependent contract
   by default, the dependent contract runs during contract checking
   because contract checking code is inside a handler
   and by effect-handler logic, it pushes the handler into the continuation
   not during the effect handler code

   --> we need to wrap the dependent contract with a handler capable of handling
   all possible effects during execution of the contract checking

   as an implementation, we unify all contract checking into one Flat (label, func, value)
   in the theoretical compiler, we can separate the effects
 *)
let f pos neg cloc (x : int) (g : (int -> int) -> int) : (int -> int) =
  let x_wrap pos neg cloc = Effect.perform (Contract.Flat (pos, check_x, x)) in
  let x_dep = x_wrap neg cloc cloc in
  let x = x_wrap neg pos cloc in

  let g_wrap pos neg cloc = fun p1 ->
    let p1_wrap pos neg cloc = fun p1_p1 ->
      let p1_p1_wrap pos neg cloc = Effect.perform (Contract.Flat (pos, check_g_p1_p1, p1_p1)) in
      let p1_p1 = p1_p1_wrap neg pos cloc in
      let body = p1 p1_p1 in
      let p1_ret_wrap pos neg cloc = Effect.perform (Contract.Flat (pos, check_g_p1_ret, body)) in
      p1_ret_wrap pos neg cloc
    in

    let p1_dep = fun p1 -> run_with_effects (fun () ->
      Printf.printf "run g_p1_dep with effects\n";
      (p1_wrap neg cloc cloc) p1)
    in
    let p1 = p1_wrap neg pos cloc in

    let body = g p1 in
    let r_wrap pos neg cloc = Effect.perform (Contract.Flat (pos, (check_g_ret p1_dep), body)) in
    r_wrap pos neg cloc
  in

  let g_dep = fun p1 -> run_with_effects (fun () ->
    Printf.printf "run g_dep with effects\n";
    (g_wrap neg cloc cloc) p1)
  in
  let g = g_wrap neg pos cloc in

  let ret =
    (* returns a value *)
    (* g x x *)

    (* returns a function *)
    (* fun y -> x + y *)
    fun y -> g (fun x -> y * x)
  in

  (* returns a value example *)
  (* let ret_wrap pos neg cloc body =
    Effect.perform (Contract.Flat (pos, check_fr_value, body))
  in
  ret_wrap pos neg cloc ret *)

  (* returns a function example *)
  let ret_wrap pos neg cloc = fun p1 ->
    let p1_wrap pos neg cloc =
      Effect.perform (Contract.Flat (pos, (check_f_p1 x_dep g_dep), p1)) in
    let p1_dep = p1_wrap neg cloc cloc in
    let p1 = p1_wrap neg pos cloc in

    let body = ret p1 in

    let r pos neg cloc =
      Effect.perform (Contract.Flat (pos, (check_f_ret x_dep g_dep p1_dep), body)) in
    r pos neg cloc
  in
  let ret = ret_wrap pos neg cloc in
  ret


let f = f "f" "main" "f"

let main = fun () ->
  let g = fun h -> h 20 in
  let h = f 10 g in
  let _ = h 100 in
  ()

let () =
  run_with_effects main
