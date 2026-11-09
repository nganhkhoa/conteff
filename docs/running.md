# Running Conteff tests and examples

Run the commands below from the repository root. The project requires OCaml 5
and Dune 3.20 or later; its PPX also requires `ppxlib` and the unit tests
require `alcotest`.

## Build

Build the runtime, PPX, expansion driver, and examples used below:

```sh
dune build lib src tests/expand examples/state examples/wrap examples/encoder examples/merkle examples/ecdh
```

To build every example, use `dune build lib src tests/expand examples` instead.
Additional dependencies are needed for some examples: `sexplib` and
`ppx_deriving` for CESK, `sexplib` and `ppx_deriving` (including its equality
plugin) for CESK proxy, `multicont` for tree traversal, and `zarith` for ECDH.
See the [CESK proxy guide](../examples/cesk-proxy/README.md) for its usage and
tests.

## Tests

Run the Alcotest unit suites and PPX expansion cram tests:

```sh
dune test tests/unit tests/expand
```

For example, run the state or dependent-contract suite individually with:

```sh
dune exec tests/unit/test_state.exe
dune exec tests/unit/test_multi_state.exe
dune exec tests/unit/test_dependent.exe
```

## Examples

Run the standalone examples with commands such as:

```sh
dune exec examples/state/increment.exe
dune exec examples/state/multiple.exe
dune exec examples/state/increasing.exe
dune exec examples/wrap/input.exe
dune exec examples/encoder/test_encoder.exe
dune exec examples/merkle/test_merkle.exe
dune exec examples/ecdh/test_ecdh_protocol.exe
dune exec examples/ecdh/test_add_points.exe
dune exec examples/ecdh/test_scalar_mult.exe
```

The ECDH server/client example uses two terminals:

```sh
dune exec examples/ecdh/server.exe -- --port 8080
dune exec examples/ecdh/client.exe -- --server 127.0.0.1:8080
```

Use `--malicious` to send an off-curve point and `--curve secp256k1`
or `--curve p256r1` to select a curve. Select the same curve on both sides.

## Main-program and state macros

Use `[%contract_main]` around the complete top-level program expression:

```ocaml
let is_positive x = x > 0

let%contract_main () =
  let value = [%mon is_positive] 1 in
  Printf.printf "%d\n" value
```

The PPX expands this binding to a call to `Conteff_lib.Contract.run`,
installing the default check handler over its right-hand side.

For a single integer state, use `[%contract_main_state initial]`:

```ocaml
open Conteff_lib

let () =
  [%contract_main_state 0]
    (Contract.set_state (Contract.get_state () + 1);
     Printf.printf "%d\n" (Contract.get_state ()))
```

It expands to `Contract.with_state ~initial (fun () -> ...)`, handling both
state and checks and discarding the final state. Use `Contract.with_state_result`
directly if the final state must be returned outside the handler. For multiple
independently typed slots, declare `[%%contract_states ...]` and use
`[%contract_main_states]`; see
[`multiple.ml`](../examples/state/multiple.ml) and
[`increasing.ml`](../examples/state/increasing.ml). Install the state handler
around applications whose checks use state, not just around construction of a
contracted function.

## Dependent contracts

For curried functions, `dep_args` names each argument for later domain and
result contracts:

```ocaml
let is_positive x = x > 0
let greater_than x y = y > x

let[@contract dep_args [(x, is_positive); (y, greater_than x)]
  (fun x y -> fun result -> result = x + y)]
  add x y = x + y
```

The argument list must be a nonempty literal list of distinct names, and the
result function must have those names as unlabelled parameters in order. It is
sugar for nested `dep` contracts, not a contract on a tuple. See the
[macro reference](macros.md) for the full syntax and behavior, including state,
blame, and handler placement, and [contract rewriting](contract-rewrite.md)
for rewriting details.

## Observe generated code

Expand one file directly:

```sh
dune exec tests/expand/pp.exe -- --impl examples/state/increment.ml
```

Save it:

```sh
dune exec tests/expand/pp.exe -- --impl examples/state/increment.ml \
  > /tmp/increment.expanded.ml
```

Regenerate the snapshots in [`macro-expansion/`](../macro-expansion/README.md)
using the commands in its README. These snapshots make generated monitor
functions, `Contract.check` calls, hijack handlers, and main-program wrappers
visible.
