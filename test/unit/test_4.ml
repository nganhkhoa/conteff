exception Blame of string

let bigger_n n num = num > n
let bigger_10 = bigger_n 10
let bigger_20 = bigger_n 20
let bigger_30 = bigger_n 30
let any _ = true

let pass g v =
  (* by the contract in f, g must be bigger_10 -> bigger_20 *)
  let vv = g (v - 20) in
  vv > 30

let [@contract : any -> ((bigger_10 -> bigger_20) -> pass dep)]
  f (x : int) (g : int -> int) : int = g x

let must_blame who =
  Alcotest.(check_raises) "must blame" (Blame who)


let test_blame_g_pre () =
  (* blame the server f *)
  (* f must gives g bigger_10, but not, blame f *)
  let g x = x in
  must_blame "Test_4" (fun () -> ignore (f 9 g))

let test_blame_g_post () =
  (* blame the client using f *)
  (* blame g, because g must outputs bigger_20 *)
  let g x = x in
  must_blame "Dune__exe__Test_4.test_blame_g_post.(fun)" (fun () -> ignore (f 19 g))

let test_blame_g_depend_pre () =
  (* blame the client using f *)
  (* g applied to 29, return 29, pass
     f outputs 29
     dependent contract calls g with 29 - 20 = 9
     does not pass the pre of g, blame contract of f
   *)
  let g x = x in
  must_blame "Test_4.f" (fun () -> ignore (f 29 g))

let test_blame_g_depend_post () =
  (* blame the client using f *)
  (* g applied to 39, return 39, pass
     f outputs 39
     dependent contract calls g with 39 - 20 = 19
     does not pass the post of g, blame g
     because we provide good argument to g
   *)
  let g x = x in
  must_blame "Dune__exe__Test_4.test_blame_g_depend_post.(fun)" (fun () -> ignore (f 39 g))

let test_blame_post () =
  (* blame f *)
  (* g applied to 49, return 49, pass
     f outputs 49
     dependent contract calls g with 49 - 20 = 29
     does not pass post requirement, blame f
   *)
  let g x = x in
  must_blame "Test_4" (fun () -> ignore (f 49 g))

let test_pass () =
  let g x = x in
  Alcotest.(check int) "Equal" 59 (f 59 g)

let () =
  let open Alcotest in
  run "Function" [
    "pass", [
      test_case "pass" `Quick test_pass;
    ];
    "blame", [
      test_case "g_pre"     `Quick test_blame_g_pre;
      test_case "g_post"    `Quick test_blame_g_post;
      test_case "g_dep_pre" `Quick test_blame_g_depend_pre;
      test_case "g_dep_post"`Quick test_blame_g_depend_post;
      test_case "dep_post"  `Quick test_blame_post;
    ];
  ]
