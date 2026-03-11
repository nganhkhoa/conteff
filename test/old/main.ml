exception Blame of string

let bigger_than_10 x =
  Printf.printf "must > 10\n";
  x > 10
let bigger_than_20 x =
  Printf.printf "must > 20\n";
  x > 20
let bigger_than_30 x =
  Printf.printf "must > 30\n";
  x > 30
let bigger_than_40 x =
  Printf.printf "must > 40\n";
  x > 40

let [@contract]
  single_value
  : int [@post : bigger_than_40] =
  x

  (*
    let module LocalContract =
      struct
        type _ Effect.t +=
          | ContractCheck__ret: ((int)[@post : bigger_than_40]) -> unit
          Effect.t
      end in

    let handler =
      {
        effc = fun (type e) (eff : e Effect.t) ->
          match eff with
          | LocalContract.ContractCheck__ret x -> Some (fun k ->
              let valid = bigger_than_40 x in
              if valid
              then continue k ()
              else discontinue k (Blame pos)
            )
          | _ -> None
      }

    in
    try_with (fun () ->
      let ret = x in
      Effect.perform (LocalContract.ContractCheck__ret ret);
      ret
    ) () handler

   *)

let
  [@contract]
  [@effect : effect_A -> check_effectA_return ]
  run
  (x : int [@pre : bigger_than_10])
  : int [@post : bigger_than_20] =
  x + 1

let [@contract] run_function pos neg
  (f : (int [@pre : bigger_than_30])
     -> (int [@post : bigger_than_40]))
  (x : int [@pre : bigger_than_20])
  : int [@post : bigger_than_10]
  =
    f x

let g x = x
let wrong g = g 11

let [@contract] test
  (f : ((int [@pre : bigger_than_20]) -> (int [@post : bigger_than_10]))
     -> (int [@post : bigger_than_10]))
  : int [@post : bigger_than_10]
  =
    (*
    let wrapped_f = fun pos neg g ->
      + "main"
      - "test"

      let wrapped_g = fun pos neg x ->
        - "test"
        + "main"
        g x
      in
      let g = wrapped_g pos neg in
    in
    let f = wrapped_f neg pos in
    *)

    (*

    + "test"
    - "main"
    let wrapped_f = fun pos neg g ->
      let (pos, neg) = neg, pos in
      + "main"
      - "test"

      let wrapped_g = fun pos neg x ->
        let (pos, neg) = neg, pos in
        + "test"
        - "main"
        g x
      in
      let g = wrapped_g pos neg in
    in
    let f = wrapped_f pos neg in
     *)
    f g

let () =
  let test = test __FUNCTION__ "test" in
  let _ = test wrong in
  ()

(* by transformation we expect it to build in multiple phases due to how
   traversal works

   1st phase resolves everything that is not a function
   for every parameters that are functions, build a wrapped version
   replace all their references into the corresponding wrapped version

   let run_function =
     let module Local = struct
       type _ Effect.t += CheckParam_x : int -> unit Effect.t
       type _ Effect.t += CheckReturn : int -> unit Effect.t
     end in

     let [@contract] f_wrapped
       (param1 : int [@pred bigger_than_10])
       : int [@pred bigger_than_10]
     = f param1 param2 ...
     in
     match
       (* body of run_function *)
       f_wrapped x
     with
     | res -> res
     | effect AllowedEffects, k -> ...

     | effect e, k ->
       match e with
       | CheckParam_x v ->
           let checked = match pred_x with
           | AllowedEffectsInContract -> ...
           in
           if checked
           then continue k ()
           else discontinue k (Blame "?")

    2nd phase expands wrapped versions using the same logic
    repeated until all functions appearing as argument are wrapped
    exception: if the user don't specify the contract, then we don't attach
    contract handler

    effects scoping clashing each other? will try with multi-layer and see
    but I expect that it is not gonna happen because each of them
    uses a different handler

    we only get problem when using handler inside a handler
 *)

(* let () =
  let open Sample_code in
  run_test *)

(* let () =
  let open Pair_projection in
  run_test *)
(*
let () =
  let run = run __FUNCTION__ "run" in
  (* let _ = run 21 in *)

  (* run_function says I will always call f with >30
     any f provided to me shud returns >40, else it's
     the provider's (server) fault
   *)
  let shud_not_good = run in

  let run_function = run_function __FUNCTION__ "run_function" in
  let _ = run_function shud_not_good 34 in
  () *)
