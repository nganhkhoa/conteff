(* Shallow runtime support for compiled contracts.
   The previous deep runtime is preserved in Contract_deep.

   The ppx compiles every flat contract check into [check], which performs
   the [Check] effect (the 𝒞 effect of the formal compiler). Higher-order
   contracts are compiled away into wrapper functions by the ppx, so the
   only thing left at runtime is: who handles [Check]?

   - [run] is the global/default handler (`wrap` in redex/compiler.rkt):
     it handles the predicate and continuation together and either resumes
     with the value or raises [Blame] at the monitor.
   - [with_policy] installs a user handler for the dynamic extent of a thunk.
     Because checks are unhandled effects at the monitor, the nearest handler
     decides what a check means: this is how a client can hijack checks
     (redex/compiler3.rkt, rule C-Hijack).

   Without a handler, [check] raises [Effect.Unhandled]. *)

type label = string

type blame = { party : label; contract : label }

exception Blame of blame

let () =
  Printexc.register_printer (function
    | Blame { party; contract } ->
        Some (Printf.sprintf "Blame(party=%S, contract=%S)" party contract)
    | _ -> None)

type _ Effect.t +=
  | Check : { pos : label; loc : label; pred : 'a -> bool; value : 'a } -> 'a Effect.t

(* The default policy for one check, used by [enforce]. *)
let rec enforce_one : 'a. pos:label -> loc:label -> ('a -> bool) -> 'a -> 'a =
  fun ~pos ~loc pred value ->
  if run (fun () -> pred value) then value
  else raise (Blame { party = pos; contract = loc })

(* The global shallow handler. Predicates are run under a fresh [run], so contracts
   used inside predicates are always enforced (h-check in the formal model
   runs the predicate under `(w bool)`). *)
and run : 'a. (unit -> 'a) -> 'a =
  fun thunk ->
  let open Effect.Shallow in
  let rec handler : type a. unit -> (a, a) handler = fun () ->
    { retc = Fun.id;
      exnc = raise;
      effc = (fun (type b) (eff : b Effect.t) ->
        match eff with
        | Check { pos; loc; pred; value } ->
            Some (fun (k : (b, a) continuation) ->
              (* Handle the predicate and the resumed continuation together. *)
              let bare =
                { retc = Fun.id; exnc = raise; effc = (fun _ -> None) } in
              continue_with
                (fiber (fun () ->
                   match pred value with
                   | true -> continue_with k value bare
                   | false ->
                       discontinue_with k
                         (Blame { party = pos; contract = loc }) bare
                   | exception e -> discontinue_with k e bare))
                () (handler ()))
        | _ -> None) }
  in
  continue_with (fiber thunk) () (handler ())

(* Emitted by the ppx for every flat contract: perform 𝒞 (pred, value). *)
let check ~pos ~loc pred value =
  Effect.perform (Check { pos; loc; pred; value })

(* An effect boundary for a function invocation. [intercept] selects operations
   using their typed constructors; its thunk runs outside this handler, so it
   can perform the operation again for the next handler. Unselected effects
   pass through unchanged. *)
type effect_contract = {
  intercept : 'a. 'a Effect.t -> (unit -> 'a) option;
}

let with_effect_contract (contract : effect_contract) thunk =
  let open Effect.Shallow in
  let rec handler : type a. unit -> (a, a) handler = fun () ->
    { retc = Fun.id;
      exnc = raise;
      effc = (fun (type b) (eff : b Effect.t) ->
        match contract.intercept eff with
        | None -> None
        | Some forward ->
            Some (fun (k : (b, a) continuation) ->
              match forward () with
              | value -> continue_with k value (handler ())
              | exception e -> discontinue_with k e (handler ()))) }
  in
  continue_with (fiber thunk) () (handler ())

(* Install a fresh boundary on every application, not when wrapping [f]. *)
let effectful_function contract f x =
  with_effect_contract contract (fun () -> f x)

(* For a single operation: the provider is responsible for its request and
   the surrounding handler for its response. [perform] reconstructs the typed
   effect from the checked request. Checks use the ordinary Check policy. *)
let forward_effect ~pos ~neg ~loc ~request ~response perform payload =
  let payload = check ~pos ~loc request payload in
  let result = perform payload in
  check ~pos:neg ~loc response result

(* ---------- custom handlers ---------- *)

(* A policy decides the outcome of one check. Returning a value resumes the
   monitored program with that value; raising aborts it at the monitor. *)
type policy = { on_check : 'a. pos:label -> loc:label -> ('a -> bool) -> 'a -> 'a }

let with_policy (p : policy) thunk =
  let open Effect.Shallow in
  let rec handler : type a. unit -> (a, a) handler = fun () ->
    { retc = Fun.id;
      exnc = raise;
      effc = (fun (type b) (eff : b Effect.t) ->
        match eff with
        | Check { pos; loc; pred; value } ->
            Some (fun (k : (b, a) continuation) ->
              match p.on_check ~pos ~loc pred value with
              | v -> continue_with k v (handler ())
              | exception e -> discontinue_with k e (handler ()))
        | _ -> None) }
  in
  continue_with (fiber thunk) () (handler ())

(* Same as the default handler. *)
let enforce = { on_check = enforce_one }

(* The hijack handler of compiler3.rkt: skip the predicate and resume with
   the value. It does not re-perform, so outer handlers never see the check. *)
let unchecked = { on_check = (fun ~pos:_ ~loc:_ _ v -> v) }

(* Hand the check to the next handler outwards. Handler code runs outside
   the handled computation, so performing here reaches the outer handler. *)
let delegate = { on_check = (fun ~pos ~loc pred v -> check ~pos ~loc pred v) }

(* Run the predicate, report failures, but never abort. *)
let warn report =
  { on_check =
      (fun ~pos ~loc pred v ->
        if run (fun () -> pred v) then v
        else (report { party = pos; contract = loc }; v)) }

(* Observe every check, then let [next] decide. *)
let observe f next =
  { on_check =
      (fun ~pos ~loc pred v ->
        f ~pos ~loc;
        next.on_check ~pos ~loc pred v) }

(* Treat every failure as blaming [party] instead. *)
let reassign party next =
  { on_check =
      (fun ~pos:_ ~loc pred v -> next.on_check ~pos:party ~loc pred v) }

let unchecked_run thunk = with_policy unchecked thunk

(* Commonly used predicates. *)
let any _ = true


(* Integer state effects. There is no mutable state cell: each handler answer
   is a state transformer, and normal return pairs its value with final state.
   Predicates run under this same protocol, including nested Check operations.
   The predicate, branch decision, and bare continuation resumption run under
   one state handler, so their updates thread through the whole computation.
   Exceptions are injected at the monitor so user code can catch Blame. *)
type _ Effect.t += GetState : int Effect.t | SetState : int -> unit Effect.t

let get_state () = Effect.perform GetState
let set_state state = Effect.perform (SetState state)

let run_state : 'a. (unit -> 'a) -> int -> 'a * int =
  fun thunk initial ->
  let open Effect.Shallow in
  let rec handler : type a. unit -> (a, int -> a * int) handler = fun () ->
    { retc = (fun value state -> (value, state));
      exnc = (fun e _state -> raise e);
      effc = (fun (type b) (eff : b Effect.t) ->
        match eff with
        | GetState ->
            Some (fun (k : (b, a) continuation) (state : int) ->
              continue_with k state (handler ()) state)
        | SetState next ->
            Some (fun (k : (b, a) continuation) (_state : int) ->
              continue_with k () (handler ()) next)
        | Check { pos; loc; pred; value } ->
            Some (fun (k : (b, a) continuation) (state : int) ->
              (* Resume k without installing another state handler. Its effects
                 are forwarded to the handler around the entire decision thunk,
                 exactly as H (fun () -> if pred value then k value else ...).
                 OCaml requires a handler argument even for this bare resume. *)
              let bare =
                { retc = Fun.id; exnc = raise; effc = (fun _ -> None) } in
              continue_with
                (fiber (fun () ->
                   match pred value with
                   | true -> continue_with k value bare
                   | false ->
                       discontinue_with k
                         (Blame { party = pos; contract = loc }) bare
                   | exception e -> discontinue_with k e bare))
                () (handler ()) state)
        | _ -> None) }
  in
  continue_with (fiber thunk) () (handler ()) initial

(* The program boundary resolves/discards state, as lambda r. lambda s. r. *)
let with_state ~initial thunk = fst (run_state thunk initial)
let with_state_result ~initial thunk = run_state thunk initial
