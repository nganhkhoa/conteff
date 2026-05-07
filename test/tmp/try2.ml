open Conteff_lib

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


let
  [@contract
  : (check_x * ((check_g_p1_p1 -> check_g_p1_ret) -> check_g_ret as 'dep)) ->
  (check_f_p1 -> check_f_ret as 'dep) as 'dep]

  (* [@effect_contract : (checking_runsomeeffect_arg -> checking_runsomeeffect) as 'Runsomeeffect] *)
  (* [@contract_handler : contract_effect_handler * contract_state * initial_contract_state] (* what about more than one? keep nesting them? *) *)
  f (x : int) (g : (int -> int) -> int) : (int -> int) =
    fun y -> g (fun x -> y * x)

let f = f "f" "main" "f"

let main = fun () ->
  let g = fun h -> h 20 in
  let h = f 10 g in
  let _ = h 100 in
  ()

let () =
  Contract.run_with_effects main
