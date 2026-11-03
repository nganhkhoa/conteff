open Effect
open Effect.Deep

type 'a tree =
  | Leaf of ('a -> bool)
  | And of 'a tree * 'a tree
  | Or of 'a tree * 'a tree

type _ Effect.t += Fork : [ `And | `Or ] -> bool Effect.t

let rec eval (v: 'a) (t : 'a tree) : bool =
  match t with
  | Leaf f -> f v
  | And (l, r) ->
      if perform (Fork `And) then eval v l else eval v r
  | Or (l, r) ->
      if perform (Fork `Or) then eval v l else eval v r

let run_multishot (v: 'a) (t : 'a tree) : bool =
  match_with (eval v) t {
    retc = (fun v -> v);
    exnc = (fun e -> raise e);
    effc = (fun (type c) (eff : c Effect.t) ->
      match eff with
      | Fork op -> Some (fun (k1 : (c, bool) continuation) ->
          let k2 = Multicont.Deep.clone_continuation k1 in

          let res_left = continue k1 true in
          let res_right = continue k2 false in

          match op with
          | `And -> res_left && res_right
          | `Or  -> res_left || res_right
        )
      | _ -> None
    )
  }

let () =
  let sample_tree_1 =
    And (
      Or (Leaf (fun x -> x < 10), Leaf (fun x -> x > 20)),
      Or (Leaf (fun x -> x > 10), Leaf (fun x -> x < 10))
    )
  in

  let sample_tree_2 =
    Or (
      And (Leaf (fun x -> x < 10), Leaf (fun x -> x > 10)),
      And (Leaf (fun x -> x < 10), Leaf (fun x -> x > 10))
    )
  in

  Printf.printf "Tree 1 Evaluation Result: %b\n" (run_multishot 1 sample_tree_1);
  Printf.printf "Tree 2 Evaluation Result: %b\n" (run_multishot 2 sample_tree_2)
