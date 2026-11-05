(* Tuple contracts: C-Mon-Tuple *)

open Conteff_lib
open Helpers

let pass () = check_pair (1, 2) (fun () -> [%mon (yes, yes)] (1, 2))

let second_fails () = check_party "k" (fun () -> [%mon (yes, is_zero) ~pos:"k"] (1, 2))

let first_fails () =
  check_blame ~party:"k" ~contract:"j" (fun () -> [%mon (no, yes) ~pos:"k" ~loc:"j"] (1, 2))

let left_to_right () =
  let seen = ref [] in
  let p x = seen := x :: !seen; true in
  ignore (Contract.run (fun () -> [%mon (p, p)] (1, 2)));
  Alcotest.(check (list int)) "order" [ 1; 2 ] (List.rev !seen)

let triple () =
  Alcotest.(check (triple int int int)) "value" (1, 2, 3)
    (Contract.run (fun () -> [%mon (yes, yes, gt 2)] (1, 2, 3)))

let nested () = check_party "k" (fun () -> [%mon ((yes, no), yes) ~pos:"k"] ((1, 2), 3))

let with_function () =
  let f, n = Contract.run (fun () ->
    [%mon (is_pos @-> yes, yes) ~pos:"k" ~neg:"l"] ((fun x -> x + 1), 3)) in
  check_int 4 (fun () -> f n);
  check_party "l" (fun () -> f (-1))

let in_function_range () =
  check_party "k" (fun () -> ([%mon (yes @-> (yes, is_pos)) ~pos:"k"] (fun x -> (x, x - 5))) 3)

let () =
  Alcotest.run "tuple"
    [ ( "tuple",
        [ case "pass" pass;
          case "second fails" second_fails;
          case "first fails" first_fails;
          case "left to right" left_to_right;
          case "triple" triple;
          case "nested" nested;
          case "with function" with_function;
          case "in function range" in_function_range ] ) ]
