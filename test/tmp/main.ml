exception Blame of string


type _ Effect.t +=
  | Dosomething : int -> int Effect.t
  | Getstate : int Effect.t
  | Runsomeeffect : int -> int Effect.t


type state = {v : int; z : int}
let initial_state = {
  v = 1;
  z = 0;
}

type contract_state = {uwu : int}
let initial_contract_state = {
  uwu = 0;
}


let checking_g'p1 v =
  Printf.printf "\tchecking g'p1 v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let checking_g'p2'p1 v =
  Printf.printf "\tchecking g'p2'p1 v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let checking_g'p2'r v =
  Printf.printf "\tchecking g'p2'r v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let checking_g'r p1 p2 v =
  Printf.printf "\tchecking g'r p1=%d v=%d uwu=%d\n" p1 v (Effect.perform Getstate);
  let uhuh = Effect.perform (Dosomething 2) in
  let _ = p2 uhuh in
  true

let checking_x v =
  Printf.printf "\tchecking x v=%d uwu=%d\n" v (Effect.perform Getstate);
  let uhuh = Effect.perform (Dosomething 1) in
  true

let checking_r g x v =
  Printf.printf "\tchecking r x=%d v=%d uwu=%d\n" x v (Effect.perform Getstate);
  Printf.printf "\t\tdependent call to argument\n";
  let _ = g v (fun y -> y + 1000) in
  true

let checking_runsomeeffect_arg v =
  Printf.printf "\tchecking Runsomeeffect argument v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let checking_runsomeeffect v =
  Printf.printf "\tchecking Runsomeeffect returns v=%d uwu=%d\n" v (Effect.perform Getstate);
  true

let contract_effect_handler = fun (type a) (type b) (eff : a Effect.t) ->
  (* Printf.printf "invoking the contract handler\n"; *)
  match eff with
  | Dosomething x -> Some (fun (k : (a, contract_state -> b) continuation) s ->
      (* Printf.printf "do something invoked %x\n" x; *)
      Effect.Deep.continue k (x + 1) {uwu = s.uwu + 1}
  )
  | Getstate -> Some (fun (k : (a, contract_state -> b) continuation) s ->
      (* Printf.printf "get state invoked %x\n" s.uwu; *)
      Effect.Deep.continue k s.uwu s
  )
  | _ -> None

let
  [@example_contract
  : ((checking_g'p1
      -> (checking_g'p2'p1 -> checking_g'p2'r)
      -> checking_g'r) dep
  -> checking_x
  -> checking_r) dep]
  [@effect_contract : (checking_runsomeeffect_arg -> checking_runsomeeffect) as 'Runsomeeffect]
  [@contract_handler : contract_effect_handler * contract_state * initial_contract_state] (* what about more than one? keep nesting them? *)
  f neg g x =

  let pos = "f" in
  let cloc = "f" in

  let module Contract = struct
    type _ Effect.t +=
      | C'g'p1 : (string * int) -> int Effect.t

      | C'g'p2'p1 : (string * int) -> int Effect.t
      (* not a dependent contract, so only the return value *)
      | C'g'p2'r  : (string * int) -> int Effect.t

      (* dependent contract will be provided with arguments *)
      | C'g'r  : (string * int * (int -> int) * int) -> int Effect.t

      | C'x    : (string * int) -> int Effect.t

      (* dependent contract will be provided with arguments *)
      | C'r    : (string * (int -> (int -> int) -> int) * int * int) -> int Effect.t
  end in

  (* contract can call effect, they have their own handler *)
  let contract_handler = {
    Effect.Deep.retc = (fun x _state -> x);
    exnc = (fun e _state -> raise e);
    effc = contract_effect_handler;
  }
  in

  (* make wrapped version of functions and value *)

  let x' pos neg cloc x = Effect.perform (Contract.C'x (pos,x)) in

  let g'p1 pos neg cloc p1 = Effect.perform (Contract.C'g'p1 (pos,p1)) in

  let g'p2'p1 pos neg cloc p1 = Effect.perform (Contract.C'g'p2'p1 (pos,p1)) in

  (* we know this g'p2 is a function receiving one argument *)
  let g'p2'r pos neg cloc p2 = fun p1 ->
      Printf.printf "[+] calling g'p2 as wrapped version\n";
      let p1 = g'p2'p1 neg pos cloc p1 in
      (* use wrapped version only *)
      let body = p2 p1 in
      Effect.perform (Contract.C'g'p2'r (pos, body))
  in

  (* we know this g is a function receiving two arguments *)
  let g' pos neg cloc g = fun p1 p2 ->
    Printf.printf "[+] calling g as wrapped version\n";
    let p1_dep = fun () ->
      Printf.printf "[+] computing p1 dep check\n";
      g'p1 neg cloc cloc p1 in
    let p1 = g'p1 neg pos cloc p1 in

    let p2_dep = g'p2'r neg cloc cloc p2 in
    let p2 = g'p2'r neg pos cloc p2 in

    (* use wrapped version only *)
    let body = g p1 p2 in
    Effect.perform (Contract.C'g'r (pos, p1_dep (), p2_dep, body))
  in

  let r pos neg cloc g x = fun () ->
    let x_dep = fun () -> x' neg cloc cloc x in
    let g_dep = g' neg cloc cloc g in

    let x = x' neg pos cloc x in
    let g = g' neg pos cloc g in

    (* use wrapped version only *)
    let body =
      let h = fun x ->
        x + 100
      in
      g x h
    in
    Effect.perform (Contract.C'r (neg, g_dep, x_dep (), body))
  in

  (* contract checking as effect

     we have a problem if we want to resolve dependent contract
     dependent contract checks when checking the result
     which is inside this handler
     by semantics of effect handler, the effect handler code
     has no handler, it install the handler in the continuation
   *)
  let rec contract_checking : type a b.
    a Effect.t
    -> ((a, b) Effect.Deep.continuation -> b) option
    = fun eff ->
    let open Effect.Deep in
    match eff with
    | Contract.C'g'p1 (label, arg) ->
        Some (fun k ->
          if checking_g'p1 arg
          then continue k arg
          else discontinue k (Blame label))
    | Contract.C'g'p2'p1 (label, arg) ->
        Some (fun k ->
          if checking_g'p2'p1 arg
          then continue k arg
          else discontinue k (Blame label))
    | Contract.C'g'p2'r (label, arg) ->
        Some (fun k ->
          if checking_g'p2'r arg
          then continue k arg
          else discontinue k (Blame label))
    | Contract.C'g'r (label,p1,p2,arg) ->
        Some (fun k ->
          let do_check = fun () -> checking_g'r p1 p2 arg in
          let ok = Effect.Deep.match_with do_check ()
            {
              Effect.Deep.retc = (fun res -> res);
              exnc = (fun e -> raise e);
              effc = contract_checking;
            }
          in
          if ok
          then continue k arg
          else discontinue k (Blame label))
    | Contract.C'x (label, arg) ->
        Some (fun k ->
          if checking_x arg
          then continue k arg
          else discontinue k (Blame label))
    | Contract.C'r (label, g,x,arg) ->
        Some (fun k ->
          let do_check = fun () -> checking_r g x arg in
          let ok = Effect.Deep.match_with do_check ()
            {
              Effect.Deep.retc = (fun res -> res);
              exnc = (fun e -> raise e);
              effc = contract_checking;
            }
          in
          if ok
          then continue k arg
          else discontinue k (Blame label))

    | Runsomeeffect x ->
        (* intercepting the main effect aka contract for effects
           to make this more programmable, put the predicate in the
           default branch, and return Some if want to wrap, else None
         *)

        (* this inherit the contract-effect handler *)
        Some (fun k ->
          if checking_runsomeeffect_arg x
          then
            let r = Effect.perform eff in
            if checking_runsomeeffect r
            then continue k r
            else discontinue k (Blame "effect ret who?")
          else discontinue k (Blame "effect arg who?"))
    | unresolved ->
        (* if we want to reject other effects, we must use discontinue
           return None will propagate the effect up
           good to have a "default" reject or allow
         *)
        None
  in

  (* handler for anything contracts *)

  (* we need two types of handlers
     1. the contract-checking handler
     2. the contract-effect handler

     the way to nest them should be important
     the contract-effect must wrap over the contract-checking

     by System Fepsilon syntax (handle h code-handled-by-h) we can define
     two ways

     1. nested handle
     handle contract-effect (handle contract-checking run-body)

     2. handle in handler

     handle contract-checking run-body
     where contract-checking = {
       check -> handle contract-effect do-check
     }

     Option 1 should be preferred, we can provide state to the
     contract-effect and contract-checking can access "states"
     by customized effects

     the problem with option 1 is that it doesn't separate the effects
     of contracts and main code, by wrapping the whole run_body with
     contract handler, it also captures the effects by run_body

     option 2 separates the effect context, and it will not interfere
     with the main code effect. BUT internal states handling is difficult
     (without mutable ref)
   *)

  (* this is the first option *)
  (*
    let main_handler =
    {
      Effect.Deep.retc = (fun v _state -> v);
      exnc = (fun e _state -> raise e);
      effc = (fun (type a) (eff : a Effect.t) ->
        (* Printf.printf "invoking the main handler\n"; *)
        (* contract_checking eff returns a function wrapped by option *)
        (contract_checking eff)
        |> Option.map (fun contract_checking_inner ->
          (fun k s ->
             Effect.Deep.match_with
               (fun () -> contract_checking_inner k s)
             ()
             contract_handler
             initial_contract_state
          ))
      );
    }
  in
  Effect.Deep.match_with run_body () main_handler initial_state *)

  (* nested effect approach *)
  let run_main () =
    Effect.Deep.match_with (r pos neg cloc g x) ()
    {
      Effect.Deep.retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = contract_checking;
    }
    (* initial_state *)
  in
  Effect.Deep.match_with run_main () contract_handler initial_contract_state

let () =
  let g = fun x h ->
    h (x + Effect.perform (Runsomeeffect 10))
  in

  (* handler for main effects, effects called during f execution *)
  let r = try (fun () -> f "main" g 1) () with
  | effect Runsomeeffect x, k ->
      let r = (x * 2) in
      Printf.printf "main effect called with %d returns %d\n" x r;
      Effect.Deep.continue k r
  in
  Printf.printf "result is %d\n" r

