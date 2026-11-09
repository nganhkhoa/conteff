Malformed uses are reported at compile time.

  $ cat > a.ml <<'X'
  > let[@contract pos] (a, b) = (1, 2)
  > X
  $ ../pp.exe --impl a.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [@contract] can only annotate a binding of a single variable

  $ cat > b.ml <<'X'
  > let x = [%mon pos]
  > X
  $ ../pp.exe --impl b.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%mon c] must be applied to an expression: [%mon c] e

  $ cat > c.ml <<'X'
  > let f = [%mon dep pos pos] g
  > X
  $ ../pp.exe --impl c.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep expects its range as (fun x -> contract)

  $ cat > d.ml <<'X'
  > let x = [%hijack]
  > X
  $ ../pp.exe --impl d.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%hijack] must be applied to an expression: [%hijack] e

  $ cat > state_missing.ml <<'X'
  > let x = [%contract_main_state] work
  > X
  $ ../pp.exe --impl state_missing.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%contract_main_state initial] expects one initial state expression

  $ cat > state_unapplied.ml <<'X'
  > let x = [%contract_main_state 0]
  > X
  $ ../pp.exe --impl state_unapplied.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%contract_main_state initial] must be applied to an expression: [%contract_main_state initial] e

  $ cat > main_payload.ml <<'X'
  > let x = [%contract_main 0] work
  > X
  $ ../pp.exe --impl main_payload.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%contract_main] does not take a payload; use [%contract_main_state initial] for state

Contract-state declarations require typed, uniquely named bindings; operations
and the entry point reject malformed payloads.

  $ cat > states_empty.ml <<'X'
  > [%%contract_states]
  > X
  $ ../pp.exe --impl states_empty.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: contract_states requires let name : type = initial [and ...]

  $ cat > states_untyped.ml <<'X'
  > [%%contract_states let a = 0]
  > X
  $ ../pp.exe --impl states_untyped.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: contract_states requires let name : type = initial

  $ cat > states_duplicate.ml <<'X'
  > [%%contract_states let a : int = 0 and a : int = 1]
  > X
  $ ../pp.exe --impl states_duplicate.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: duplicate contract state name

  $ cat > get_payload.ml <<'X'
  > let x = [%contract_get_a 1]
  > X
  $ ../pp.exe --impl get_payload.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%contract_get_a] does not take a payload

  $ cat > set_missing.ml <<'X'
  > let x = [%contract_set_a]
  > X
  $ ../pp.exe --impl set_missing.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: expected an expression in the state extension

  $ cat > states_main_unapplied.ml <<'X'
  > let x = [%contract_main_states]
  > X
  $ ../pp.exe --impl states_main_unapplied.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%contract_main_states] must be applied to a body

Malformed dep_args contracts must be rejected by the expander.

  $ cat > dep_args_empty.ml <<'X'
  > let f = [%mon dep_args [] (fun x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_empty.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args requires at least one argument

  $ cat > dep_args_duplicate.ml <<'X'
  > let f = [%mon dep_args [(x, yes); (x, yes)] (fun x x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_duplicate.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args requires distinct argument names

  $ cat > dep_args_not_literal.ml <<'X'
  > let f = [%mon dep_args args (fun x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_not_literal.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects a literal list of (identifier, contract) entries

  $ cat > dep_args_tail.ml <<'X'
  > let f = [%mon dep_args ((x, yes) :: rest) (fun x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_tail.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects a literal list of (identifier, contract) entries

  $ cat > dep_args_entry.ml <<'X'
  > let f = [%mon dep_args [yes] (fun x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_entry.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects entries of the form (identifier, contract)

  $ cat > dep_args_binder.ml <<'X'
  > let f = [%mon dep_args [(1, yes)] (fun x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_binder.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects entries of the form (identifier, contract)

  $ cat > dep_args_result.ml <<'X'
  > let f = [%mon dep_args [(x, yes); (y, yes)] result] g
  > X
  $ ../pp.exe --impl dep_args_result.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects (fun x y ... -> contract) with the listed names in order

  $ cat > dep_args_order.ml <<'X'
  > let f = [%mon dep_args [(x, yes); (y, yes)] (fun y x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_order.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects (fun x y ... -> contract) with the listed names in order

  $ cat > dep_args_missing_param.ml <<'X'
  > let f = [%mon dep_args [(x, yes); (y, yes)] (fun x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_missing_param.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects (fun x y ... -> contract) with the listed names in order

  $ cat > dep_args_extra_param.ml <<'X'
  > let f = [%mon dep_args [(x, yes)] (fun x y -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_extra_param.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects (fun x y ... -> contract) with the listed names in order

  $ cat > dep_args_pattern.ml <<'X'
  > let f = [%mon dep_args [(x, yes)] (fun (x, y) -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_pattern.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects (fun x y ... -> contract) with the listed names in order

  $ cat > dep_args_label.ml <<'X'
  > let f = [%mon dep_args [(x, yes)] (fun ~x -> yes)] g
  > X
  $ ../pp.exe --impl dep_args_label.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects (fun x y ... -> contract) with the listed names in order

  $ cat > dep_args_arity.ml <<'X'
  > let f = [%mon dep_args [(x, pos)]] succ
  > X
  $ ../pp.exe --impl dep_args_arity.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep_args expects a list and a result function
