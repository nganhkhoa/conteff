(* Dependent function contracts: C-Mon-Dep, indy blame. Ported from the
   dependent cases of redex/compiler.rkt. *)

open Helpers

let succ x = x + 1

let pass () =
  check_int 11 (fun () -> ([%mon dep yes (fun y -> fun r -> r - y = 1)] succ) 10)

let range_fail () =
  check_blame ~party:"k" ~contract:"j" (fun () ->
      ([%mon dep yes (fun y -> fun r -> r - y = 0) ~pos:"k" ~neg:"l" ~loc:"j"] succ) 10)

let domain_fail () =
  check_party "l" (fun () -> ([%mon dep is_pos (fun y -> gt y) ~pos:"k" ~neg:"l"] succ) (-3))

let binder_same_as_parameter () =
  let f = [%mon dep yes (fun x -> fun r -> r > x) ~pos:"k"] (fun x -> x * 2) in
  check_int 10 (fun () -> f 5);
  check_party "k" (fun () -> f (-5))

let function_range_pass () =
  check_int 3 (fun () -> ([%mon dep yes (fun y -> yes @-> fun r -> r > y)] (fun a b -> a + b)) 1 2)

let function_range_fail () =
  check_party "k" (fun () ->
      ([%mon dep yes (fun y -> yes @-> fun r -> r > y) ~pos:"k"] (fun a b -> a + b)) 1 (-2))

let indy_blames_contract () =
  (* the range contract misuses its dependent argument g: with indy blame the
     contract itself (j) is at fault, not the server or the client *)
  let server = [%mon dep (is_pos @-> yes) (fun g -> fun r -> g (-1) <> r)
                  ~pos:"k" ~neg:"l" ~loc:"j"] (fun g -> g 1)
  in
  check_blame ~party:"j" ~contract:"j" (fun () -> server (fun x -> x))

let dependent_higher_order_pass () =
  let server = [%mon dep (is_pos @-> yes) (fun _ -> yes) ~pos:"k" ~neg:"l" ~loc:"j"] (fun g -> g 1) in
  (* the client's g returns fine; the server's use of g is fine too *)
  check_int 1 (fun () -> server (fun x -> x))

let range_uses_each_argument () =
  let f = [%mon dep yes (fun y -> fun r -> r = y * y) ~pos:"k"] (fun x -> if x = 3 then 10 else x * x) in
  check_int 4 (fun () -> f 2);
  check_party "k" (fun () -> f 3);
  check_int 16 (fun () -> f 4)

let dependent_variable_unused () =
  check_int 2 (fun () -> ([%mon dep yes (fun _ -> is_pos)] succ) 1)

let () =
  Alcotest.run "dependent"
    [ ( "dependent",
        [ case "pass" pass;
          case "range fail" range_fail;
          case "domain fail" domain_fail;
          case "binder same as parameter" binder_same_as_parameter;
          case "function range pass" function_range_pass;
          case "function range fail" function_range_fail;
          case "indy blames contract" indy_blames_contract;
          case "dependent higher-order pass" dependent_higher_order_pass;
          case "range uses each argument" range_uses_each_argument;
          case "dependent variable unused" dependent_variable_unused ] ) ]
