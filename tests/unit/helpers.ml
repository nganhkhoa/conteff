(* Shared assertions for the contract tests. Every test body runs under the
   global handler [Contract.run], as a compiled program would. *)

open Conteff_lib

let blame =
  Alcotest.testable
    (fun ppf (b : Contract.blame) ->
      Format.fprintf ppf "{party=%S; contract=%S}" b.party b.contract)
    ( = )

let blamed f =
  match Contract.run f with
  | _ -> Alcotest.fail "expected a contract violation"
  | exception Contract.Blame b -> b

let check_blame ~party ~contract f =
  Alcotest.check blame "blame" { Contract.party; contract } (blamed f)

let check_party party f = Alcotest.(check string) "blamed party" party (blamed f).party

let check_int expected f = Alcotest.(check int) "value" expected (Contract.run f)

let check_bool expected f = Alcotest.(check bool) "value" expected (Contract.run f)

let check_pair expected f = Alcotest.(check (pair int int)) "value" expected (Contract.run f)

let check_ints expected f = Alcotest.(check (list int)) "value" expected (Contract.run f)

(* predicates used throughout, as in the redex tests *)
let yes _ = true
let no _ = false
let is_pos x = x > 0
let is_zero x = x = 0
let gt n x = x > n
let lt n x = x < n
let is_even x = x mod 2 = 0

let case name f = Alcotest.test_case name `Quick f
