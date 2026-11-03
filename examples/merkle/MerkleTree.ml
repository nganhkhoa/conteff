type tree =
  | Empty
  | Leaf of bytes
  | Node of bytes * tree * tree

exception Blame of string

let any _ = true

let good_length b = Bytes.length b = 32

(* Dependent Predicate: Resulting tree must have same leaf count as input data *)
let rec count_leaves (tr : tree) : int =
  match tr with
  | Empty -> 0
  | Leaf _ -> 1
  | Node (_, l, r) -> (count_leaves l) + (count_leaves r)

let equal_count items tr =
  count_leaves tr = List.length items

let hash_node h left_tree right_tree =
  let get_hash = function
    | Empty -> Bytes.empty
    | Leaf h | Node (h, _, _) -> h
  in
  let combined = Bytes.cat (get_hash left_tree) (get_hash right_tree) in
  h (Bytes.to_string combined)

let
  [@contract : (any -> good_length) -> (any -> equal_count dep)]
  build (h : string -> bytes) (items : string list) : tree =
    let rec build_rec = function
      | [] -> Empty
      | [x] -> Leaf (h x)
      | xs ->
          let mid = List.length xs / 2 in
          let left_list, right_list =
            let rec split i acc = function
              | l when i = 0 -> List.rev acc, l
              | x :: tx -> split (i - 1) (x :: acc) tx
              | [] -> List.rev acc, []
            in split mid [] xs
          in
          let l = build_rec left_list in
          let r = build_rec right_list in
          Node (hash_node h l r, l, r)
    in
    build_rec items
