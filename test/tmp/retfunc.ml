let pos = "f"
let neg = "main"
let cloc = "f"
exception Blame of string

let
  (* [@contract
  : any -> (somecheck -> somecheck2)] *)
  f (x : int) : (int -> int) =
  (* let h = fun x ->
    x + 100
  in
  h *)

  Printf.printf "run f\n";

  let module Contract = struct
    type _ Effect.t +=
      | C'x'      : (string * int) -> int Effect.t
      | C'r'p1'   : (string * int) -> int Effect.t
      | C'r'r'    : (string * int) -> int Effect.t
  end in

  let x' pos neg cloc x = Effect.perform (Contract.C'x' (pos, x)) in
  let r'p1' pos neg cloc p1 = Effect.perform (Contract.C'r'p1' (pos, p1)) in
  let r'r' pos neg cloc r = fun p1 ->
    let p1 = r'p1' neg pos cloc p1 in
    let body = r p1 in
    Effect.perform (Contract.C'r'r' (pos, body))
  in

  let run_body pos neg cloc x = fun () ->
    let x = x' neg pos cloc x in
    let body =
      let h = fun x ->
        x + 100
      in
      h
    in
    r'r' pos neg cloc body
  in

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

  (* this works for basic value, it computes this function enclosed in effects *)
  let run_main () =
    Effect.Deep.match_with (run_body pos neg cloc x) ()
    {
      Effect.Deep.retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = contract_checking;
    }
  in

  (* we return with a function, we need to wrap it with handler
     so that this function has wrapped effects
   *)
  fun p1 ->
    (* run_main will gives u a function and we provide all arguments to it
       when it is run, it will have the contract checked defined here
     *)
    Effect.Deep.match_with (fun () -> (run_main ()) p1) ()
    {
      Effect.Deep.retc = (fun x -> x);
      exnc = (fun e -> raise e);
      effc = contract_checking;
    }

let () =
  let g = f 10 in
  let _ = g 10 in
  ()
