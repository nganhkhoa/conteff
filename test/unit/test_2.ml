exception Blame of string

let bigger_n n num = num > n
let bigger_10 = bigger_n 10
let bigger_20 = bigger_n 20
let bigger_30 = bigger_n 30
let any _ = true

let [@contract : bigger_10] x : int = 9

let [@contract : bigger_10 -> bigger_20] f (x : int) : int = x

let must_blame who =
  Alcotest.(check_raises) "must blame" (Blame who)


let test_blame_input () =
  (* blame who provides 9 *)
  must_blame "Dune__exe__Test_2.test_blame_input.(fun)" (fun () -> ignore (f 9))

let test_blame_output () =
  (* blame f, which is the module providing f *)
  must_blame "Test_2" (fun () -> ignore (f 19))

let test_pass () =
  Alcotest.(check int) "Equal" 21 (f 21)

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

