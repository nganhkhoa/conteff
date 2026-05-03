let pos = "f"
let neg = "main"
let cloc = "f"
exception Blame of string

module Contract = struct
  type _ Effect.t +=
    | C'x'      : (string * int) -> int Effect.t
    | C'r'p1'   : (string * int) -> int Effect.t
    | C'r'r'    : (string * int) -> int Effect.t
end


let run_with_effects thunk =
  let rec contract_checking : type a b.
    a Effect.t
    -> ((a, b) Effect.Deep.continuation -> b) option
    = fun eff ->
    let open Effect.Deep in
    Printf.printf "do check\n";
    match eff with
    | Contract.C'x' (label, arg) ->
        Some (fun k ->
          if true
          then continue k arg
          else discontinue k (Blame label))
    | Contract.C'r'p1' (label, arg) ->
        Some (fun k ->
          if true
          then continue k arg
          else discontinue k (Blame label))
    | Contract.C'r'r' (label, arg) ->
        Some (fun k ->
          if true
          then continue k arg
          else discontinue k (Blame label))
    | _ -> None
  in

  Effect.Deep.match_with thunk ()
  {
    Effect.Deep.retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = contract_checking;
  }


let
  (* [@contract
  : any -> (somecheck -> somecheck2)] *)
  f (x : int) : (int -> int) =

  (* computes nothing, this is a thunk *)

  let x' pos neg cloc x = Effect.perform (Contract.C'x' (pos, x)) in
  let r'p1' pos neg cloc p1 = Effect.perform (Contract.C'r'p1' (pos, p1)) in
  let r'r' pos neg cloc r = fun p1 ->
    let p1 = r'p1' neg pos cloc p1 in
    let body = r p1 in
    Effect.perform (Contract.C'r'r' (pos, body))
  in

  (* real computation here *)
  let r pos neg cloc x =
    let x = x' neg pos cloc x in
    let body =
      let h = fun x ->
        x + 100
      in
      h
    in
    (* because body is a function *)
    r'r' pos neg cloc body
  in

  (* if body returns a function it must be wrapped and run_with_effects *)
  fun p1 -> run_with_effects (fun () -> (r pos neg cloc x) p1)


let f p1 = run_with_effects (fun () -> f p1)

let () =
  let g = f 10 in
  let _ = g 10 in
  ()

