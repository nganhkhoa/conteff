(* Higher-order union contracts, following effect-racket's examples/union.rkt.
   Run from the impl root: dune exec examples/capabilities/union_contracts.exe

   Like Moy's example, this defines its own contract projections and installs
   them at a contract boundary using [custom]. Branch state is held by a
   dynamically scoped state handler, like box-service in effect-racket. The
   Conteff state handler also handles contract checks. *)
open Conteff_lib

(* A functional box store inside Conteff's handler state. Integer box IDs are
   allocated by the handler; each box stores its surviving child indices.
   The handler must surround both projection and every subsequent call, just as
   effect-racket's [with (box-service)] does. *)
[%%contract_states
  let union_store : int * (int * int list) list = (0, [])]

let new_branch count =
  let next, boxes = [%contract_get_union_store] in
  let children = List.init count Fun.id in
  [%contract_set_union_store (next + 1, (next, children) :: boxes)];
  next

let update_branch id polarity failed child =
  let next, boxes = [%contract_get_union_store] in
  let children = List.assoc id boxes in
  let children =
    if failed = polarity then List.filter ((<>) child) children else [] in
  let boxes = List.map (fun (key, old) ->
    if key = id then (key, children) else (key, old)) boxes in
  [%contract_set_union_store (next, boxes)];
  children = []

type polarity = Provider | Client

let flip = function Provider -> Client | Client -> Provider

(* [fail] is the stem-error/branch-error protocol. Each child method closes
   over its branch and its identity. A same-polarity failure removes that
   child; an opposite-polarity failure invalidates the whole branch. Empty
   branches propagate their own polarity to their parent, while preserving the
   original failing polarity for the final blame. *)
type stem = { fail : polarity -> polarity -> unit }

let blame ~provider ~client ~loc = function
  | Provider -> raise (Contract.Blame { party = provider; contract = loc })
  | Client -> raise (Contract.Blame { party = client; contract = loc })

let make_branch ~parent ~polarity count =
  let id = new_branch count in
  Array.init count (fun i ->
    { fail = (fun failed original ->
        if update_branch id polarity failed i then parent.fail polarity original) })

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
          (fun v -> if not (pred v) then parent.fail polarity polarity; true)
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

(* A Conteff custom projection receives the boundary's actual labels. *)
let union_projection contract ~pos ~neg ~loc value =
  let root = { fail = (fun _ original -> blame ~provider:pos ~client:neg ~loc original) } in
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
  [%mon (custom (union_projection contract)) ~pos:"provider" ~neg:"client" ~loc] value

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
  [%contract_main_states] (test_f ());
  [%contract_main_states] (test_g ());
  [%contract_main_states] (test_h1 ());
  [%contract_main_states] (test_h2 ());
  print_endline "union_contracts: all assertions passed"

(* The handler must remain active for the lifetime of projected functions;
   escaping its scope or invoking them under a different store is unsupported.
   As in the upstream example, eliminated wrappers remain stacked and predicate
   effects are not rolled back. This is not a contract-effect isolation or
   concurrency guarantee. Custom Check policies can bypass checks. *)
