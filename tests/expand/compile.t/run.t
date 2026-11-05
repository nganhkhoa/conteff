The ppx output for each compile rule of redex/compiler.rkt.

C-Mon-Flat: a flat contract is a perform, no handler.

  $ cat > flat.ml <<'X'
  > let x = [%mon is_zero ~pos:"k" ~neg:"l" ~loc:"j"] 0
  > X
  $ ../pp.exe --impl flat.ml
  let x =
    let __conteff_pred1 = is_zero in
    Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred1 0

C-Mon-Func: wrapper lambdas, labels swapped for the argument.

  $ cat > func.ml <<'X'
  > let f = [%mon (yes @-> no) ~pos:"k" ~neg:"l" ~loc:"j"] g
  > X
  $ ../pp.exe --impl func.ml
  let f =
    let __conteff_pred1 = yes in
    let __conteff_pred2 = no in
    let __conteff_f3 = g in
    fun __conteff_x4 ->
      Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred2
        (__conteff_f3
           (Conteff_lib.Contract.check ~pos:"l" ~loc:"j" __conteff_pred1
              __conteff_x4))

C-Mon-Dep: the argument is monitored a second time with (l, j) for the range.

  $ cat > dep.ml <<'X'
  > let f = [%mon dep yes (fun y -> gt y) ~pos:"k" ~neg:"l" ~loc:"j"] g
  > X
  $ ../pp.exe --impl dep.ml
  let f =
    let __conteff_pred1 = yes in
    let __conteff_f2 = g in
    fun __conteff_x3 ->
      let y =
        Conteff_lib.Contract.check ~pos:"l" ~loc:"j" __conteff_pred1
          __conteff_x3 in
      let __conteff_pred4 = gt y in
      Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred4
        (__conteff_f2
           (Conteff_lib.Contract.check ~pos:"l" ~loc:"j" __conteff_pred1
              __conteff_x3))

C-Mon-Tuple

  $ cat > tuple.ml <<'X'
  > let p = [%mon (yes, no) ~pos:"k" ~neg:"l" ~loc:"j"] (1, 2)
  > X
  $ ../pp.exe --impl tuple.ml
  let p =
    let __conteff_pred1 = yes in
    let __conteff_pred2 = no in
    let (__conteff_v3, __conteff_v4) = (1, 2) in
    let __conteff_r5 =
      Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred1 __conteff_v3 in
    let __conteff_r6 =
      Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred2 __conteff_v4 in
    (__conteff_r5, __conteff_r6)

C-Hijack: a handler around the compiled expression.

  $ cat > hijack.ml <<'X'
  > let a = [%hijack] ([%mon no ~pos:"k" ~neg:"l" ~loc:"j"] 1)
  > let b = [%hijack my_policy] e
  > X
  $ ../pp.exe --impl hijack.ml
  let a =
    Conteff_lib.Contract.with_policy Conteff_lib.Contract.unchecked
      (fun () ->
         let __conteff_pred1 = no in
         Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred1 1)
  let b = Conteff_lib.Contract.with_policy my_policy (fun () -> e)

A contracted binding: a monitor function over the raw value, use sites
supply the negative label, and the name is exported for other modules.

  $ cat > binding.ml <<'X'
  > let[@contract pos @-> pos] f (x : int) : int = x
  > let client () = f 1
  > X
  $ ../pp.exe --impl binding.ml
  let __conteff_raw_f_2 (x : int) : int= x
  let __conteff_mon_f_1 __conteff_neg3 =
    let __conteff_pred4 = pos in
    let __conteff_pred5 = pos in
    let __conteff_f6 = __conteff_raw_f_2 in
    fun __conteff_x7 ->
      Conteff_lib.Contract.check ~pos:"Binding.f" ~loc:"Binding.f"
        __conteff_pred5
        (__conteff_f6
           (Conteff_lib.Contract.check ~pos:__conteff_neg3 ~loc:"Binding.f"
              __conteff_pred4 __conteff_x7))
  let f __conteff_a8 = __conteff_mon_f_1 "external" __conteff_a8[@@warning
                                                                  "-32"]
  let client () = (__conteff_mon_f_1 "Binding.client") 1

C-Main: the program entry point is wrapped in the default contract handler.

  $ cat > main.ml <<'X'
  > let is_pos x = x > 0
  > let () = [%contract_main] (ignore ([%mon is_pos ~pos:"p" ~loc:"m"] 1))
  > X
  $ ../pp.exe --impl main.ml
  let is_pos x = x > 0
  let () =
    Conteff_lib.Contract.run
      (fun () ->
         ignore
           (let __conteff_pred1 = is_pos in
            Conteff_lib.Contract.check ~pos:"p" ~loc:"m" __conteff_pred1 1))

C-Main-binding: let%contract_main annotates the program entry point.

  $ cat > main_binding.ml <<'X'
  > let is_pos x = x > 0
  > let%contract_main () = ignore ([%mon is_pos ~pos:"p" ~loc:"m"] 1)
  > X
  $ ../pp.exe --impl main_binding.ml
  let is_pos x = x > 0
  let () =
    Conteff_lib.Contract.run
      (fun () ->
         ignore
           (let __conteff_pred1 = is_pos in
            Conteff_lib.Contract.check ~pos:"p" ~loc:"m" __conteff_pred1 1))

State-aware entry points accept an initial expression and wrap the body in a
single state-aware handler, rather than nesting the ordinary main handler.

  $ cat > main_state.ml <<'X'
  > let x = [%contract_main_state (initial ())] ([%mon is_pos] value)
  > let y = [%contract_main_state 42] (work ())
  > X
  $ ../pp.exe --impl main_state.ml
  let x =
    Conteff_lib.Contract.with_state ~initial:(initial ())
      (fun () ->
         let __conteff_pred1 = is_pos in
         Conteff_lib.Contract.check ~pos:"server" ~loc:"main_state.ml:1"
           __conteff_pred1 value)
  let y = Conteff_lib.Contract.with_state ~initial:42 (fun () -> work ())

A group of contract states generates distinct operations and one shared shallow
handler. Initializers are retained, and operations in the main body are rewritten.

  $ cat > contract_states.ml <<'X'
  > [%%contract_states let a : int = initial_a () and b : string = initial_b ()]
  > let result = [%contract_main_states] (let x = [%contract_get_a] in [%contract_set_a (x + 1)]; [%contract_get_b])
  > X
  $ ../pp.exe --impl contract_states.ml > expanded.ml
  $ grep -E '^(type _ Effect.t|  \| Conteff_contract_state_|let __conteff_contract_state_|let __conteff_contract_states_run|let result)' expanded.ml
  type _ Effect.t +=  
    | Conteff_contract_state_a_Get: int Effect.t 
    | Conteff_contract_state_a_Set: int -> unit Effect.t 
  let __conteff_contract_state_a_get () =
  let __conteff_contract_state_a_set value =
  type _ Effect.t +=  
    | Conteff_contract_state_b_Get: string Effect.t 
    | Conteff_contract_state_b_Set: string -> unit Effect.t 
  let __conteff_contract_state_b_get () =
  let __conteff_contract_state_b_set value =
  let __conteff_contract_states_run thunk =
  let result =
  $ grep -E 'initial_[ab] \(\)|Effect.Shallow|Conteff_lib.Contract.Check|__conteff_contract_states_run|__conteff_contract_state_[ab]_(get|set)' expanded.ml
  let __conteff_contract_state_a_get () =
  let __conteff_contract_state_a_set value =
  let __conteff_contract_state_b_get () =
  let __conteff_contract_state_b_set value =
  let __conteff_contract_states_run thunk =
    let __conteff_contract_initial_0 = initial_a () in
    let __conteff_contract_initial_1 = initial_b () in
    let open Effect.Shallow in
                 | Conteff_lib.Contract.Check { pos; loc; pred; value } ->
    __conteff_contract_states_run
         let x = __conteff_contract_state_a_get () in
         __conteff_contract_state_a_set (x + 1);
         __conteff_contract_state_b_get ())
