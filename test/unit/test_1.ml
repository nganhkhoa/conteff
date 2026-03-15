exception Blame of string

let bigger_10 num = num > 10

let [@contract : bigger_10] x : int = 9
let [@contract : bigger_10] y : int = 11

let must_blame who =
  Alcotest.(check_raises) "must blame" (Blame who)


let test_blame_use_variable () =
  must_blame "Test_1" (fun () -> ignore x)

let test_blame_use_variable_in_function () =
  let f () = x + 1 in
  must_blame "Test_1" (fun () -> ignore (f ()))

let test_pass_use_variable () =
  Alcotest.(check int) "Equal" 11 y

let test_pass_use_variable_in_function () =
  let f () = y + 10 in
  Alcotest.(check int) "Equal" 21 (f ())

let () =
  let open Alcotest in
  run "Value" [
    "pass", [
      test_case "value" `Quick test_pass_use_variable;
      test_case "function" `Quick test_pass_use_variable_in_function
    ];
    "blame", [
      test_case "value" `Quick test_blame_use_variable;
      test_case "function" `Quick test_blame_use_variable_in_function
    ];
  ]
