open Effect.Deep

exception Blame of string

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

let run_with_effects thunk =
  Effect.Deep.match_with thunk ()
  {
    Effect.Deep.retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = contract_checking;
  }

