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

(* [dep_args] is syntactic sugar for nested dependent contracts. *)
let dep_args_one_argument () =
  let f = [%mon dep_args [(x, is_pos)] (fun x -> fun r -> r = x + 1)
                  ~pos:"k" ~neg:"l" ~loc:"j"] succ in
  check_int 3 (fun () -> f 2);
  check_blame ~party:"l" ~contract:"j" (fun () -> f 0)

let dep_args_multiple_domains_and_range () =
  let f = [%mon dep_args [(x, is_pos); (y, gt x); (z, gt (x + y))]
                 (fun x y z -> fun r -> r = x + y + z)
                 ~pos:"k" ~neg:"l" ~loc:"j"]
      (fun x y z -> if z = 8 then 0 else x + y + z) in
  check_int 10 (fun () -> f 1 2 7);
  check_blame ~party:"l" ~contract:"j" (fun () -> f 0 2 7);
  check_blame ~party:"l" ~contract:"j" (fun () -> f 2 2 7);
  check_blame ~party:"l" ~contract:"j" (fun () -> f 1 2 3);
  check_blame ~party:"k" ~contract:"j" (fun () -> f 1 2 8)

let dep_args_lexical_shadowing () =
  (* The first domain sees the outer x; the second sees the argument x.
     The result's local x shadows the dependent binder. *)
  let x = 10 in
  let f = [%mon dep_args [(x, gt x); (y, gt x)]
                 (fun x y -> let x = x + 100 in fun r -> r = x + y)
                 ~pos:"k" ~neg:"l" ~loc:"j"]
      (fun x y -> x + 100 + y) in
  check_int 123 (fun () -> f 11 12);
  check_blame ~party:"l" ~contract:"j" (fun () -> f 10 12);
  check_blame ~party:"l" ~contract:"j" (fun () -> f 11 11)

let dep_args_partial_application () =
  let calls = ref 0 in
  let f = [%mon dep_args [(x, is_pos); (y, gt x)]
                 (fun x y -> fun r -> r = x + y)
                 ~pos:"k" ~neg:"l" ~loc:"j"]
      (fun x y -> incr calls; x + y) in
  let p = Conteff_lib.Contract.run (fun () -> f 2) in
  let q = Conteff_lib.Contract.run (fun () -> f 5) in
  Alcotest.(check int) "body not yet called" 0 !calls;
  check_int 5 (fun () -> p 3);
  check_blame ~party:"l" ~contract:"j" (fun () -> q 3);
  check_int 11 (fun () -> q 6);
  check_blame ~party:"l" ~contract:"j" (fun () -> p 2);
  Alcotest.(check int) "only valid calls reach body" 2 !calls;
  check_blame ~party:"l" ~contract:"j" (fun () -> f 0)

let dep_args_higher_order_blame () =
  let good = [%mon dep_args [(g, is_pos @-> is_pos); (x, is_pos)]
                    (fun g x -> fun r -> r = g x)
                    ~pos:"k" ~neg:"l" ~loc:"j"]
      (fun g x -> g x) in
  check_int 2 (fun () -> good (fun x -> x + 1) 1);
  check_blame ~party:"l" ~contract:"j" (fun () -> good (fun _ -> 0) 1);
  let bad = [%mon dep_args [(g, is_pos @-> yes); (x, is_pos)]
                   (fun g x -> fun r -> r = g x)
                   ~pos:"k" ~neg:"l" ~loc:"j"]
      (fun g _ -> g (-1)) in
  check_blame ~party:"k" ~contract:"j" (fun () -> bad (fun x -> x) 1)

let dep_args_higher_order_indy () =
  let f = [%mon dep_args [(g, is_pos @-> yes); (x, is_pos)]
                 (fun g x -> fun r -> g (-x) <> r)
                 ~pos:"k" ~neg:"l" ~loc:"j"]
      (fun g x -> g x) in
  check_blame ~party:"j" ~contract:"j" (fun () -> f (fun x -> x) 1)

let dep_args_higher_order_later_domain_indy () =
  let f = [%mon dep_args [(g, is_pos @-> yes); (x, fun x -> g (-1) = x)]
                 (fun g x -> fun r -> r = x && g x = r)
                 ~pos:"k" ~neg:"l" ~loc:"j"]
      (fun _ x -> x) in
  check_blame ~party:"j" ~contract:"j" (fun () -> f (fun x -> x) 1)

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
          case "dependent variable unused" dependent_variable_unused ] );
      ( "dep_args",
        [ case "one argument" dep_args_one_argument;
          case "multiple domains and range" dep_args_multiple_domains_and_range;
          case "lexical shadowing" dep_args_lexical_shadowing;
          case "partial application" dep_args_partial_application;
          case "higher-order blame" dep_args_higher_order_blame;
          case "higher-order range indy" dep_args_higher_order_indy;
          case "higher-order later domain indy" dep_args_higher_order_later_domain_indy ] ) ]
