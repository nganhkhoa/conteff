# Running Conteff tests and examples

## Build

Build the libraries, PPX, tests, and examples with:

```sh
dune build lib src tests/expand examples
```

The `cesk` and `tree-traversal` examples may require optional external
libraries such as `multicont`.

## Tests

Run the Alcotest unit suites and PPX expansion cram tests:

```sh
dune test tests/unit tests/expand
```

## Examples

Run the standalone examples with commands such as:

```sh
dune exec examples/state/increment.exe
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

Use `--malicious` to exercise contract rejection and `--curve secp256k1`
or `--curve p256r1` to select a curve.

## Main-program macro

Use `[%contract_main]` around the complete top-level program expression:

```ocaml
let%contract_main () =
  let value = [%mon is_positive] 1 in
  Printf.printf "%d\n" value
```

The PPX expands it to a call to `Conteff_lib.Contract.run`, installing the
default contract handler over the entire expression. This is preferable to
manually writing `Contract.run` in executable examples.

For stateful code, use `[%contract_main_state initial]` instead of `[%contract_main]`:

```ocaml
let () =
  [%contract_main_state 0]
    (let result = program () in
     let state = Contract.get_state () in
     Printf.printf "%d/%d\n" result state)
```

It expands to `Contract.with_state ~initial (fun () -> ...)`, handling both
state and checks and discarding the final state. Use `Contract.with_state_result`
directly if the final state must be returned outside the handler.

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

Regenerate all checked-in snapshots in `macro-expansion/` using the commands
in `macro-expansion/README.md`. The snapshots make monitor functions, flat
`Contract.check` calls, hijack handlers, and the global `[%contract_main]` wrapper
visible.
