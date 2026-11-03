(* Runtime support for compiled contracts.

   The ppx compiles every flat contract check into [check], which performs
   the [Check] effect (the 𝒞 effect of the formal compiler). Higher-order
   contracts are compiled away into wrapper functions by the ppx, so the
   only thing left at runtime is: who handles [Check]?

   - [run] is the global/default handler (`wrap` in redex/compiler.rkt):
     it runs the predicate under a fresh default handler and either resumes
     with the value or raises [Blame] at the monitor.
   - [with_policy] installs a user handler for the dynamic extent of a thunk.
     Because checks are unhandled effects at the monitor, the nearest handler
     decides what a check means: this is how a client can hijack checks
     (redex/compiler3.rkt, rule C-Hijack).

   When no handler is installed at all, [check] falls back to the default
   behaviour, as if the whole program ran under [run]. *)

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

(* The default policy for one check, used by [run] and the fallback. *)
let rec enforce_one : 'a. pos:label -> loc:label -> ('a -> bool) -> 'a -> 'a =
  fun ~pos ~loc pred value ->
  if run (fun () -> pred value) then value
  else raise (Blame { party = pos; contract = loc })

(* The global handler. Predicates are run under a fresh [run], so contracts
   used inside predicates are always enforced (h-check in the formal model
   runs the predicate under `(w bool)`). *)
and run : 'a. (unit -> 'a) -> 'a =
  fun thunk ->
  let open Effect.Deep in
  try_with thunk ()
    { effc =
        (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Check { pos; loc; pred; value } ->
              Some
                (fun (k : (b, _) continuation) ->
                  match enforce_one ~pos ~loc pred value with
                  | v -> continue k v
                  | exception e -> discontinue k e)
          | _ -> None) }

(* Emitted by the ppx for every flat contract: perform 𝒞 (pred, value). *)
let check ~pos ~loc pred value =
  try Effect.perform (Check { pos; loc; pred; value })
  with Effect.Unhandled (Check _) -> enforce_one ~pos ~loc pred value

(* ---------- custom handlers ---------- *)

(* A policy decides the outcome of one check. Returning a value resumes the
   monitored program with that value; raising aborts it at the monitor. *)
type policy = { on_check : 'a. pos:label -> loc:label -> ('a -> bool) -> 'a -> 'a }

let with_policy (p : policy) thunk =
  let open Effect.Deep in
  try_with thunk ()
    { effc =
        (fun (type b) (eff : b Effect.t) ->
          match eff with
          | Check { pos; loc; pred; value } ->
              Some
                (fun (k : (b, _) continuation) ->
                  match p.on_check ~pos ~loc pred value with
                  | v -> continue k v
                  | exception e -> discontinue k e)
          | _ -> None) }

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
