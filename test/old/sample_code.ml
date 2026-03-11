open Effect.Deep

exception Blame of string

type _ Effect.t += GetAdditionOperand : int Effect.t
(* type _ Effect.t += CheckAdditionOperand : int -> bool Effect.t *)
type _ Effect.t += SomeContractEffect : unit Effect.t

(* suggestion syntax

let [@contract] f

  (x : t [@@pred ...] [@@allow-effect ...])
  (y : t [@@pred ...])
  : t [@@pred ...]
      [@@allow-effect ...]

  = body

 *)

let bigger_than_10 n =
  Effect.perform SomeContractEffect;
  n > 10

let bigger_than_20 n =
  n > 20

let equal_10 n =
  n = 10

let f = fun x ->
  let module Local = struct
    type _ Effect.t += CheckParam1 : int -> unit Effect.t
    type _ Effect.t += CheckReturn : int -> int Effect.t
  end in
  match
    Effect.perform (Local.CheckParam1 x);

    (* f can also do other effects, and we can constraint it
       allowing certain effects, and if the effect returns,
       must be able to "validate" it using contract

       prob need a verbose version of this, with handlers
       we can check if effect is one of ours Contract checking
       or the allowed effects

      infact, how verbose is the effect filter?
      if we only allow certain effect, then only allow their effect
      but if we also need to check the effect input, then we need
      a predicate for that input checking
     *)

    let num = Effect.perform GetAdditionOperand in
    let ret = x + num in
    Effect.perform (Local.CheckReturn ret)
  with
  | res -> res

  (* besides contract effects, only allow certain effects

     Main Contract Handler, perform the effect again to push
     to parent (registered) handler
   *)
  | effect GetAdditionOperand, k ->
      (* we catch this effect to check its result *)
      let num = Effect.perform GetAdditionOperand in
      Printf.printf "F Main Effect Check: %d\n" num;
      (* only allowed values passing the predicate to pass *)
      if equal_10 num
      then continue k num
      else discontinue k (Blame "F does not pass the effect filter")

  (* else we only need to care about Contract checking for parameters
     built as effects
   *)
  | effect e, k ->
      match
        begin
        match e with
        (* this also register a Contract-Effect Handler

           This contract (predicate) can perform certain effects, and disallow (?) the rest
           this contract also has a guard (for effect request)

           state?
         *)
        | Local.CheckParam1 v ->
            Printf.printf "F Check Param 1: %d\n" v;
            (* Contract-Effect Handler installed

               what if we want to keep a state? bigger_than_10 calls SomeContractEffect
               many times, and want to compare results across effect calls
               we can use a state to control, regardless, this handler
               is written by the user, and we can let the user define their state
               the param check only calls once though

               but if param1 param2 calls the same effect and wants to track between them?
               what about return value?
             *)
            let check_passed =
              match bigger_than_10 v with
              | res -> res
              | effect SomeContractEffect, kk ->
                  Printf.printf "Contract can call effect\n";
                  continue kk ()
            in
            if check_passed
            then continue k ()
            else discontinue k (Blame "F Check Param 1 contract error")

        | Local.CheckReturn v ->
            Printf.printf "F Check Return Value: %d\n" v;
            if bigger_than_20 v
            then continue k v
            else discontinue k (Blame "F Return Value Not Good")

        | _ ->
            discontinue k (Blame "F effect disallow")
        end
      with
      | res -> res
      | effect GetAdditionOperand, k ->
          (* Contract-Handler Contract
             if the contract perform other effects, this catches
           *)
          discontinue k (Blame "F some other effects called during contract checking --> disallowed")

let g = fun f x ->
  (* g can specify signature for f too
     we wrap f again, by default, it should perform as we would
     expect in normal contract checking
   *)
  let f_binded = fun x ->
    let module Local = struct
      type _ Effect.t += CheckParam1 : int -> unit Effect.t
      type _ Effect.t += CheckReturn : int -> int Effect.t
    end in
    let predicate_1 v = v < 15 in
    let predicate_ret v = v < 25 in
    match
      Effect.perform (Local.CheckParam1 x);
      let ret = f x in
      Effect.perform (Local.CheckReturn ret)
    with
    | res -> res
    | effect (Local.CheckParam1 v), k ->
        Printf.printf "G Check Param 1: %d\n" v;
        if (predicate_1 v)
        then continue k ()
        else discontinue k (Blame "G Param 1 Not Good")
    | effect (Local.CheckReturn v), k ->
        Printf.printf "G Check Return Value: %d\n" v;
        if (predicate_ret v)
        then continue k v
        else discontinue k (Blame "G Return Value Not Good")
  in
  f_binded x

let run_test =
  match
    let num = read_int () in
    let _ = g f num in
    ()
  with
  | res -> res
  | effect GetAdditionOperand, k ->
      continue k 10
