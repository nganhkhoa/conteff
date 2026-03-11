open Effect.Deep

exception Blame of string

type _ Effect.t += Pos : string Effect.t
type _ Effect.t += Neg : string Effect.t

(* type _ Effect.t += GetAdditionOperand : int Effect.t *)
(* type _ Effect.t += CheckAdditionOperand : int -> bool Effect.t *)
(* type _ Effect.t += SomeContractEffect : unit Effect.t *)

let bigger_than_10 n =
  n > 10

let bigger_than_20 n =
  n > 20

let equal_10 n =
  n = 10

let f = fun pos neg x ->
  let module Local = struct
    type _ Effect.t += CheckParam1 : int -> unit Effect.t
    type _ Effect.t += CheckReturn : int -> int Effect.t
  end in
  match
    Effect.perform (Local.CheckParam1 x);
    let ret = x + 10 in
    Effect.perform (Local.CheckReturn ret)
  with
  | res -> res
  | effect e, k ->
      match
        begin
        match e with
        | Local.CheckParam1 v ->
            Printf.printf "F Check Param 1: %d\n" v;
            if bigger_than_10 v
            then continue k ()
            else discontinue k (Blame neg)

        | Local.CheckReturn v ->
            Printf.printf "F Check Return Value: %d\n" v;
            if bigger_than_20 v
            then continue k v
            else discontinue k (Blame pos)

        | _ ->
            discontinue k (Blame "F effect disallow")
        end
      with
      | res -> res

let g = fun pos neg f x ->
  let f_binded = fun pos neg x ->
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
        else discontinue k (Blame neg)
    | effect (Local.CheckReturn v), k ->
        Printf.printf "G Check Return Value: %d\n" v;
        if (predicate_ret v)
        then continue k v
        else discontinue k (Blame pos)
  in
  let f = f_binded neg pos in
  f x

(* return a function?


let h = fun pos neg ->
  let f_binded = fun pos neg x ->
    let module Local = struct
      type _ Effect.t += CheckParam1 : int -> unit Effect.t
      type _ Effect.t += CheckReturn : int -> int Effect.t
    end in
    let predicate_1 v = v < 15 in
    let predicate_ret v = v < 25 in
    match
      Effect.perform (Local.CheckParam1 x);
      let ret = f 16 in
      Effect.perform (Local.CheckReturn ret)
    with
    | res -> res
    | effect (Local.CheckParam1 v), k ->
        Printf.printf "G Check Param 1: %d\n" v;
        if (predicate_1 v)
        then continue k ()
        else discontinue k (Blame neg)
    | effect (Local.CheckReturn v), k ->
        Printf.printf "G Check Return Value: %d\n" v;
        if (predicate_ret v)
        then continue k v
        else discontinue k (Blame pos)
  in
  f_binded pos "h" *)

let run_test =
  let g = g "g" "main_g" in
  let f = f "f" "main_f" in
  let _ = g f 17 in
  ()
