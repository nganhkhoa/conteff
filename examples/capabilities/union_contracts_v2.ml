(* Higher-order union contracts with an explicit, shallow effect handler.
   This file uses no contract PPX. Run from the impl root:
   dune exec examples/capabilities/union_contracts_v2.exe. The handler must enclose projection AND use. *)
open Conteff_lib

type store = int * (int * int list) list

type _ Effect.t += Get_union_store : store Effect.t
                 | Set_union_store : store -> unit Effect.t

let get_store () = Effect.perform Get_union_store
let set_store store = Effect.perform (Set_union_store store)

(* The answer type is a state transformer. In particular, a Check predicate,
   its decision, and the resumed continuation all run under the SAME handler.
   Discontinuing at the original check allows a surrounding [try] to catch
   blame without discarding state updates made before the exception. *)
let run_union_store : 'a. (unit -> 'a) -> 'a * store = fun thunk ->
  let open Effect.Shallow in
  let rec handler : type a. unit -> (a, store -> a * store) handler = fun () ->
    { retc = (fun value state -> value, state);
      exnc = (fun exn _state -> raise exn);
      effc = (fun (type b) (eff : b Effect.t) ->
        match eff with
        | Get_union_store ->
            Some (fun (k : (b, a) continuation) (state : store) ->
              continue_with k state (handler ()) state)
        | Set_union_store next ->
            Some (fun (k : (b, a) continuation) (_state : store) ->
              continue_with k () (handler ()) next)
        | Contract.Check { pos; loc; pred; value } ->
            Some (fun (k : (b, a) continuation) (state : store) ->
              let bare =
                { retc = Fun.id; exnc = raise; effc = (fun _ -> None) } in
              continue_with
                (fiber (fun () ->
                   match pred value with
                   | true -> continue_with k value bare
                   | false ->
                       discontinue_with k
                         (Contract.Blame { party = pos; contract = loc }) bare
                   | exception exn -> discontinue_with k exn bare))
                () (handler ()) state)
        | _ -> None) }
  in
  continue_with (fiber thunk) () (handler ()) (0, [])

let with_union_store thunk = ignore (run_union_store thunk)

let new_branch count =
  let next, boxes = get_store () in
  let children = List.init count Fun.id in
  set_store (next + 1, (next, children) :: boxes);
  next

let update_branch id polarity failed child =
  let next, boxes = get_store () in
  let children = List.assoc id boxes in
  let children =
    if failed = polarity then List.filter ((<>) child) children else [] in
  let boxes = List.map (fun (key, old) ->
    if key = id then (key, children) else (key, old)) boxes in
  set_store (next, boxes);
  children = []

type polarity = Provider | Client

let flip = function Provider -> Client | Client -> Provider

(* Explicit parent links model Moy's stem and branch nodes. The surviving
   child set lives in the handler store, not in a mutable tree node. *)
type stem = Root of (polarity -> unit) | Stem of branch * int
and branch = { id : int; polarity : polarity; parent : stem }

let blame ~provider ~client ~loc = function
  | Provider -> raise (Contract.Blame { party = provider; contract = loc })
  | Client -> raise (Contract.Blame { party = client; contract = loc })

let rec stem_error stem failed original =
  match stem with
  | Root report -> report original
  | Stem (branch, child) ->
      if update_branch branch.id branch.polarity failed child then
        stem_error branch.parent branch.polarity original

let make_branch ~parent ~polarity count =
  let branch = { id = new_branch count; polarity; parent } in
  Array.init count (fun i -> Stem (branch, i))

(* OCaml's type system enforces the shape of function contracts. Disjunctions
   must have at least one alternative; heterogeneous values can be represented
   by a variant, as in the tests below. *)
type _ contract =
  | Flat : ('a -> bool) -> 'a contract
  | Arrow : 'a contract * 'b contract -> ('a -> 'b) contract
  | Union : 'a contract list -> 'a contract

let rec project : type a. a contract -> polarity -> stem -> a -> a =
  fun contract polarity parent value ->
    match contract with
    | Flat pred ->
        (* Route flat observations through Conteff's ordinary Check effect.
           A failed alternative is recorded rather than immediately blamed. *)
        Contract.check ~pos:"union observation" ~loc:"union observation"
          (fun v -> if not (pred v) then stem_error parent polarity polarity; true)
          value
    | Arrow (domain, range) ->
        fun argument ->
          let argument = project domain (flip polarity) parent argument in
          let result = value argument in
          project range polarity parent result
    | Union alternatives ->
        if alternatives = [] then invalid_arg "union: no alternatives";
        let stems = make_branch ~parent ~polarity (List.length alternatives) in
        (* Like the Racket projection, stack all wrappers, sharing the branch
           across calls, rather than selecting a disjunct on each call. *)
        List.fold_left2
          (fun value alternative stem -> project alternative polarity stem value)
          value alternatives (Array.to_list stems)

(* Explicitly install the projection at a boundary, without PPX syntax. *)
let union_projection contract ~pos ~neg ~loc value =
  let root = Root (blame ~provider:pos ~client:neg ~loc) in
  project contract Provider root value

let expect_blame party loc f =
  match f () with
  | _ -> failwith ("expected blame: " ^ party ^ "/" ^ loc)
  | exception Contract.Blame { party = p; contract = l } ->
      assert (p = party && l = loc)

type value = Num of int | Str of string | Other

let number = Flat (function Num _ -> true | _ -> false)
let positive = Flat (function Num n -> n > 0 | _ -> false)
let string = Flat (function Str _ -> true | _ -> false)
let loc = "number-or-positive function"
let protect contract value =
  union_projection contract ~pos:"provider" ~neg:"client" ~loc value

(* Moy's three examples. Each test group runs in a fresh state handler;
   calls within a group deliberately share state, especially h1 and h2. *)
let test_f () =
  let f = protect (Arrow (number, number)) (function
    | Num 0 -> Str ""
    | x -> x) in
  assert (f (Num 10) = Num 10);
  expect_blame "client" loc (fun () -> f (Str ""));
  expect_blame "provider" loc (fun () -> f (Num 0))

let test_g () =
  let either = Union [number; string] in
  let g = protect (Arrow (either, either)) (function
    | Num n when n > 0 -> Str (string_of_int n)
    | Str s -> (match int_of_string_opt s with Some n -> Num n | None -> Other)
    | _ -> Other) in
  assert (g (Num 5) = Str "5");
  assert (g (Str "5") = Num 5);
  expect_blame "client" loc (fun () -> g Other);
  expect_blame "provider" loc (fun () -> g (Num (-5)))

let make_h () =
  let c = Union [Arrow (number, string); Arrow (positive, number)] in
  protect c (function
    | Num n when n mod 2 = 0 -> Str (string_of_int n)
    | Num n -> Num n
    | _ -> failwith "body received invalid input")

let test_h1 () =
  let h = make_h () in
  assert (h (Num 10) = Str "10");
  expect_blame "client" loc (fun () -> h (Num (-10)));
  expect_blame "provider" loc (fun () -> h (Num 13))

let test_h2 () =
  let h = make_h () in
  assert (h (Num 13) = Num 13);
  expect_blame "provider" loc (fun () -> h (Num 10))

let () =
  with_union_store test_f;
  with_union_store test_g;
  with_union_store test_h1;
  with_union_store test_h2;
  print_endline "union_contracts_v2: all assertions passed"

(* The handler must remain active for the lifetime of projected functions;
   escaping its scope or invoking them under a different store is unsupported.
   As in the upstream example, eliminated wrappers remain stacked and predicate
   effects are not rolled back. This is not a contract-effect isolation or
   concurrency guarantee. Custom Check policies can bypass checks. *)
