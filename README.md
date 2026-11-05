# CONtract To EFFects

This project tries to implement software contracts using effects and effect handlers. OCaml has support for effects, but not contracts, which is an ideal choice for implementation.

## Use

Add this project to the dune dependency.

```
opam pin add conteff git+https://github.com/nganhkhoa/conteff.git
```

```dune
; dune-project
(package
 (name your_package)
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.0))
  (conteff)
 ))
```

```dune
; dune
(executable
 (name main)
 (public_name app)
 (preprocess (pps conteff)))
```

Annotate bindings with contracts, or monitor expressions directly. More details are in
[./docs/contract-rewrite.md](./docs/contract-rewrite.md).

```ocaml
let is_pos x = x > 0

(* flat, function, dependent, tuple, box contracts *)
let[@contract is_pos] x : int = 1
let[@contract is_pos @-> is_pos] f x = x + 10
let[@contract dep is_pos (fun x -> fun r -> r > x)] g x = x + 1

(* the formal (mon k l j κ e) *)
let h = [%mon (is_pos @-> is_pos) ~pos:"server" ~neg:"client" ~loc:"h"] (fun x -> x)

(* run under the global handler; violations raise Conteff_lib.Contract.Blame *)
let () = Conteff_lib.Contract.run (fun () -> print_int (f 1))

(* hijack the checks in the dynamic extent of an expression *)
let y = [%hijack] (f (-1))                         (* checks skipped *)
let z = [%hijack Conteff_lib.Contract.warn report] (f (-1))
```

## Build and test

```sh
$ dune build lib src
$ dune test tests/unit tests/expand

# show what the ppx produces for a file
$ dune exec tests/expand/pp.exe -- --impl tests/unit/test_let.ml
```

`tests/unit` holds the Alcotest suites. `tests/expand` holds cram tests of the ppx output
for each compile rule. Runnable programs are under `examples/`; the previous implementation
is kept under `legacy/` and is not built.

### Handler-placement example

`examples/wrap/input.ml` contrasts a handler around creation of a protected function with
a handler around its application. Its generated PPX code and runtime output are checked in:

- `examples/wrap/output/expanded.ml`
- `examples/wrap/output/run.txt`

Regenerate them with:

```sh
$ dune exec tests/expand/pp.exe -- --impl examples/wrap/input.ml \
    > examples/wrap/output/expanded.ml
$ dune exec examples/wrap/input.exe > examples/wrap/output/run.txt
```

### ECDH Example

```sh
$ dune build examples/ecdh

# some unit tests
$ dune exec examples/ecdh/test_add_points.exe
$ dune exec examples/ecdh/test_scalar_mult.exe
$ dune exec examples/ecdh/test_ecdh_protocol.exe

# working server/client ECDH key exchange
$ dune exec examples/ecdh/server.exe
$ dune exec examples/ecdh/client.exe
```

Useful parameters for `server` and `client` to test:

- `--malicious` will send malform point bytes
- `--curve curvename` will change curve point generated

Using these params, point decode will crash, the contract should catch.

### Shallow handlers and contract-controlled state

`Conteff_lib.Contract` now uses `Effect.Shallow`, explicitly reinstalling the
handler when resuming. The previous deep runtime is preserved unchanged as
`Conteff_lib.Contract_deep` (its effects and blame exception are distinct from
those of `Contract`; PPX-generated monitors still use `Contract`).

Stateful predicates use `Contract.get_state ()` and `Contract.set_state n`.
Install the state-aware check handler around **function applications**, not
only around construction of the contracted function:

```ocaml
let increasing_input input =
  let previous = Contract.get_state () in
  if input < previous then false
  else (Contract.set_state input; true)

let[@contract increasing_input @-> Contract.any] f x = x + 100

let result = [%contract_main_state 0] (
  let a = f 2 in
  let b = f 5 in
  (a, b))
```

`[%contract_main_state initial]` expands to `with_state`
and discards final state; `with_state_result` returns `(result, state)`.
The initial integer is an explicit prior input for an input-history contract.
State is passed through handler answers rather than a mutable reference.
Nested checks in predicates run under the same state protocol and their updates
reach both the enclosing predicate and the resumed program. Ordinary `run` and
policy handlers retain their prior enforcement behavior; for shared predicate
state, use `with_state` directly rather than installing an intervening policy
handler. State operations without a state handler are unhandled effects.

A false predicate injects `Blame` at the monitor with the predicate's final
state. If the predicate itself raises an exception, it is also injected at the
monitor. Predicate evaluation, branch selection, and continuation resumption
share one handler scope, so state updates before either failure remain visible
to code that catches the exception. State is not rolled back.

Run the state tests and the increment example:

```sh
dune exec tests/unit/test_state.exe
dune exec examples/state/increment.exe
# Results: (11, 21)
# Calls: 2
```

The increment example's function returns `x + 1`; its always-true domain
predicate also increments the state exactly once per call. The codomain is
unrestricted, avoiding an extra counter update for the result check.

### Multiple named contract states

An optional whole-file pass supports independently typed state slots and a
single shallow handler for both state operations and contract checks:

```ocaml
[%%contract_states
  let calls : int = 0
  and last : string = ""]

let count name =
  [%contract_set_calls ([%contract_get_calls] + 1)];
  [%contract_set_last name];
  true

let[@contract count @-> Contract.any] greet name = "Hello " ^ name

let () = [%contract_main_states] (
  ignore (greet "Ada");
  Printf.printf "%d: %s\n" [%contract_get_calls] [%contract_get_last])
```

Use an OCaml structure extension and underscore names: hyphens in the proposed
extension names are parsed as subtraction by OCaml. Each slot requires an
explicit type. Declare the slots before their uses, once per module; the pass
operates on one compilation unit and does not discover states across files.
Initializers run once per invocation, in declaration order, and the handler
returns the body result (discarding the final states). Read final values inside
the body when needed. State operations outside the generated handler are
unhandled. The existing `[%contract_main_state initial]` remains available for
single integer state.

For a full reference to the macro syntax, expansions, state handlers, and blame
labels, see [docs/macros.md](docs/macros.md).
