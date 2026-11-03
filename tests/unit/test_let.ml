(* let[@contract c] bindings: the server is the binding, the client is the
   definition containing each use. *)

open Conteff_lib
open Helpers

let[@contract is_pos @-> gt 10] f (x : int) : int = x + 5

let[@contract : is_pos -> is_pos] old_syntax (x : int) : int = x

let above x r = r > x
let[@contract : is_pos -> above dep] bump (x : int) : int = if x = 5 then x else x + 1

let[@contract yes @-> yes] id x = x

let[@contract is_pos @-> is_pos] rec countdown n = if n <= 1 then n else countdown (n - 2)

let[@contract is_pos @-> is_pos] rec fact n = if n <= 1 then 1 else n * fact (n - 1)

module Server = struct
  let[@contract is_pos @-> is_pos] g (x : int) : int = x * 2
  let local_use () = g (-1)
end

let pass () = check_int 11 (fun () -> f 6)

let server_blamed () = check_blame ~party:"Test_let.f" ~contract:"Test_let.f" (fun () -> f 1)

let test_client () = check_blame ~party:"Test_let.test_client" ~contract:"Test_let.f" (fun () -> f (-1))

let caller_a () = f (-1)
let caller_b () = f (-2)

let distinct_use_sites () =
  check_party "Test_let.caller_a" caller_a;
  check_party "Test_let.caller_b" caller_b

let first_class_use () = check_ints [ 11; 12 ] (fun () -> List.map f [ 6; 7 ])

let first_class_use_fail () = check_party "Test_let.first_class_use_fail" (fun () -> List.map f [ 6; 0 ])

let old_syntax_test () = check_party "Test_let.old_syntax_test" (fun () -> old_syntax 0)

let old_dep_syntax () =
  check_int 2 (fun () -> bump 1);
  check_party "Test_let.bump" (fun () -> bump 5);
  check_party "Test_let.old_dep_syntax" (fun () -> bump 0)

let polymorphic () =
  check_int 1 (fun () -> id 1);
  Alcotest.(check string) "string" "s" (Contract.run (fun () -> id "s"))

let recursive_pass () = check_int 120 (fun () -> fact 5)

let recursive_calls_monitored () =
  (* countdown 4 recurs on 0: the recursive call site, inside countdown, is
     the client that broke the contract *)
  check_party "Test_let.countdown" (fun () -> countdown 4);
  check_int 1 (fun () -> countdown 3);
  check_party "Test_let.recursive_calls_monitored" (fun () -> countdown 0)

let module_path () = check_blame ~party:"Test_let.Server.local_use" ~contract:"Test_let.Server.g" Server.local_use

let external_use () =
  (* a qualified reference goes through the exported binding *)
  check_blame ~party:"external" ~contract:"Test_let.Server.g" (fun () -> Server.g (-1))

let local_flat_fail () =
  check_blame ~party:"Test_let.local_flat_fail.n" ~contract:"Test_let.local_flat_fail.n" (fun () ->
      let[@contract is_even] n = 3 in
      n + 1)

let local_flat_pass () =
  check_int 5 (fun () ->
      let[@contract is_even] n = 4 in
      n + 1)

let local_function () =
  check_party "Test_let.local_function" (fun () ->
      let[@contract is_pos @-> yes] h x = x in
      h 0)

let shadowed_by_let () =
  check_int (-1) (fun () ->
      let f x = x in
      f (-1))

let shadowed_by_parameter () =
  let apply f = f (-1) in
  check_int (-1) (fun () -> apply (fun x -> x))

let shadowed_by_match () =
  check_int (-1) (fun () -> match (fun x -> x) with f -> f (-1))

let hijacked () = check_int 4 (fun () -> [%hijack] (f (-1)))

let () =
  Alcotest.run "let"
    [ ( "let",
        [ case "pass" pass;
          case "server blamed" server_blamed;
          case "client blamed" test_client;
          case "distinct use sites" distinct_use_sites;
          case "first-class use" first_class_use;
          case "first-class use fail" first_class_use_fail;
          case "type payload syntax" old_syntax_test;
          case "type payload dep syntax" old_dep_syntax;
          case "polymorphic" polymorphic;
          case "recursive pass" recursive_pass;
          case "recursive calls monitored" recursive_calls_monitored;
          case "module path" module_path;
          case "external use" external_use;
          case "local flat fail" local_flat_fail;
          case "local flat pass" local_flat_pass;
          case "local function" local_function;
          case "shadowed by let" shadowed_by_let;
          case "shadowed by parameter" shadowed_by_parameter;
          case "shadowed by match" shadowed_by_match;
          case "hijacked" hijacked ] ) ]
