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


let checking_g'p1 state v =
  Printf.printf "\tchecking g'p1 v=%d state.v=%d uwu=%d\n" v state.v (Effect.perform Getstate);
  ({state with v = 2}, true)

let checking_g'r state v =
  Printf.printf "\tchecking g'r v=%d state.v=%d uwu=%d\n" v state.v (Effect.perform Getstate);
  let uhuh = Effect.perform (Dosomething 2) in
  ({state with v = uhuh + 3}, true)

let checking_x state v =
  Printf.printf "\tchecking x v=%d state.v=%d uwu=%d\n" v state.v (Effect.perform Getstate);
  let uhuh = Effect.perform (Dosomething 1) in
  ({state with v = uhuh}, true)

let checking_r state v =
  Printf.printf "\tchecking r v=%d state.v=%d uwu=%d\n" v state.v (Effect.perform Getstate);
  ({state with v = 0}, true)

let checking_runsomeeffect state v =
  Printf.printf "\tchecking Runsomeeffect v=%d state.v=%d uwu=%d\n" v state.v (Effect.perform Getstate);
  ({state with v = 9}, true)

let pos = "f"
let neg = "main"

let f g x =
  let module Contract = struct
    type _ Effect.t +=
      | C'g'p1 : int -> int Effect.t
      | C'g'r  : int -> int Effect.t
      | C'x    : int -> int Effect.t
      | C'r    : int -> int Effect.t
  end in

  (* contract can call effect, they have their own handler *)
  let contract_handler = {
    Effect.Deep.retc = (fun x _state -> x);
    exnc = (fun e _state -> raise e);
    effc = (fun (type a) (type b) (eff : a Effect.t) ->
      (* Printf.printf "invoking the contract handler\n"; *)
      match eff with
      | Dosomething x -> Some (fun (k : (a, contract_state -> b) continuation) s ->
          Printf.printf "do something invoked %x\n" x;
          Effect.Deep.continue k (x+1) {uwu = s.uwu + 1}
      )
      | Getstate -> Some (fun (k : (a, contract_state -> b) continuation) s ->
          Printf.printf "get state invoked %x\n" x;
          Effect.Deep.continue k s.uwu s
      )
      | _ -> None)
  }
  in

  (* contract checking as effect *)
  let contract_checking (type a) (eff : a Effect.t)
    : ((a, 'b) Effect.Deep.continuation -> 'b) option =
    let open Effect.Deep in
    match eff with
    | Contract.C'g'p1 arg ->
        Some (fun k s ->
          let s', ok = checking_g'p1 s arg in
          if ok
          then continue k arg s'
          else discontinue k (Blame pos) s')
    | Contract.C'g'r arg ->
        Some (fun k s ->
          let s', ok = checking_g'r s arg in
          if ok
          then continue k arg s'
          else discontinue k (Blame neg) s')
    | Contract.C'x arg ->
        Some (fun k s ->
          let s', ok = checking_x s arg in
          if ok
          then continue k arg s'
          else discontinue k (Blame neg) s')
    | Contract.C'r arg ->
        Some (fun k s ->
          let s', ok = checking_r s arg in
          if ok
          then continue k arg s'
          else discontinue k (Blame pos) s')
    | Runsomeeffect x ->
        (* intercepting the main effect aka contract for effects
           to make this more programmable, put the predicate in the
           default branch, and return Some if want to wrap, else None
         *)

        (* this inherit the contract-effect handler *)
        Some (fun k s ->
          let r = Effect.perform (Runsomeeffect x) in
          let s', ok = checking_runsomeeffect s r in
          if ok
          then continue k r s'
          else discontinue k (Blame "who?") s')
    | unresolved ->
        (* if we want to reject other effects, we must use discontinue
           return None will propagate the effect up
           good to have a "default" reject or allow
         *)
        None
  in

  let run_body = fun () ->
    let x = Effect.perform (Contract.C'x x) in
    let g_wrapped p1 =
      let p1' = Effect.perform (Contract.C'g'p1 p1) in
      Effect.perform (Contract.C'g'r (g p1'))
    in
    Effect.perform (Contract.C'r (g_wrapped x))
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
    Effect.Deep.match_with run_body ()
    {
      Effect.Deep.retc = (fun x _state -> x);
      exnc = (fun e _state -> raise e);
      effc = contract_checking;
    }
    initial_state
  in
  Effect.Deep.match_with run_main () contract_handler initial_contract_state

let () =
  let g = fun x ->
    x + Effect.perform (Runsomeeffect 10)
  in

  (* handler for main effects, effects called during f execution *)
  let r = try (fun () -> f g 1) () with
  | effect Runsomeeffect x, k ->
      Printf.printf "main effect called\n";
      Effect.Deep.continue k (x + 1)
  in
  Printf.printf "result is %d\n" r

