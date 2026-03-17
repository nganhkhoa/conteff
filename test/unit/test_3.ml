exception Blame of string

let bigger_n n num = num > n
let bigger_10 = bigger_n 10
let bigger_20 = bigger_n 20
let bigger_30 = bigger_n 30
let any _ = true

let total_bigger_50 x v = bigger_n 50 (x + v)

let [@contract : bigger_10 -> total_bigger_50 dep] f (x : int) : int = x

let must_blame who =
  Alcotest.(check_raises) "must blame" (Blame who)


let test_blame_input () =
  (* blame the client using f *)
  must_blame "Dune__exe__Test_3.test_blame_input.(fun)" (fun () -> ignore (f 9))

let test_blame_output () =
  (* blame f the server *)
  must_blame "Test_3" (fun () -> ignore (f 25))

let test_pass () =
  Alcotest.(check int) "Equal" 26 (f 26)

let () =
  let open Alcotest in
  run "Function" [
    "pass", [
      test_case "pass" `Quick test_pass;
    ];
    "blame", [
      test_case "input" `Quick test_blame_input;
      test_case "output" `Quick test_blame_output;
    ];
  ]


