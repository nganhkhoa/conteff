let positive x = x > 0
let protected =
  let __conteff_pred1 = positive in
  let __conteff_pred2 = positive in
  let __conteff_f3 x = x in
  fun __conteff_x4 ->
    Conteff_lib.Contract.check ~pos:"provider" ~loc:"positive -> positive"
      __conteff_pred2
      (__conteff_f3
         (Conteff_lib.Contract.check ~pos:"client"
            ~loc:"positive -> positive" __conteff_pred1 __conteff_x4))
let show_blame thunk =
  try Printf.printf "result: %d\n" (thunk ())
  with
  | Conteff_lib.Contract.Blame { party; contract } ->
      Printf.printf "blame: party=%s, contract=%s\n" party contract
let () =
  let escaped =
    Conteff_lib.Contract.with_policy Conteff_lib.Contract.unchecked
      (fun () -> protected) in
  Printf.printf "handler around creation: ";
  show_blame (fun () -> escaped (-1));
  Printf.printf "handler around application: ";
  show_blame
    (fun () ->
       Conteff_lib.Contract.with_policy Conteff_lib.Contract.unchecked
         (fun () -> protected (-1)));
  Printf.printf "default run around application: ";
  show_blame (fun () -> Conteff_lib.Contract.run (fun () -> protected (-1)))
