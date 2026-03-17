exception Blame of string

let bigger_n n num = num > n
let bigger_10 = bigger_n 10
let bigger_20 = bigger_n 20
let bigger_30 = bigger_n 30
let any _ = true

let pass g v =
  (* by the contract in f, g must be bigger_10 -> bigger_20 *)
  let _ = g v in
  true

let [@contract : (bigger_10 -> bigger_20) -> pass dep]
  f (g : int -> int) : int = g 11

let must_blame who =
  Alcotest.(check_raises) "must blame" (Blame who)


let test_blame_g_pre () =
  (* blame the client using f *)
  (* when dependent contract is run, g is called with 9,
     breaking its pre contract *)
  let g x = 9 in
  must_blame "Dune__exe__Test_4.test_blame_g_pre.(fun)" (fun () -> ignore (f g))

let test_blame_g_post () =
  (* blame the client using f *)
  (* when dependent contract is run, g is called with 19,
     breaking its post contract *)
  let g x = 19 in
  must_blame "Dune__exe__Test_4.test_blame_g_post.(fun)" (fun () -> ignore (f g))

let test_pass () =
  let g x = 21 in
  Alcotest.(check int) "Equal" 21 (f g)

let () =
  let open Alcotest in
  run "Function" [
    "pass", [
      test_case "pass" `Quick test_pass;
    ];
    "blame", [
      test_case "g_pre" `Quick test_blame_g_pre;
      test_case "g_post" `Quick test_blame_g_post;
    ];
  ]



