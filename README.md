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
  (ocaml (>= 4.14.0))
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

Annotate the desire function/variables with contracts. More details in [./docs/contract-rewrite.md](./docs/contract-rewrite.md).

```ocaml
(* bigger_0 and bigger_10 is defined correctly else compile error *)

let [@contract : bigger_0] x = 1

let [@contract : bigger_0 -> bigger_10] f x = x + 10
```

## Build and test

```sh
$ dune build lib
$ dune runtest test/unit

# manual tests are defined here to test the rewritter
$ dune describe pp test/manual/somefile.ml
```

### ECDH Examples

```sh
$ dune build test/ecdh

# some unit tests
$ dune exec test/ecdh/test_add_points.exe
$ dune exec test/ecdh/test_scalar_mult.exe
$ dune exec test/ecdh/test_ecdh_protocol.exe

# working server/client ECDH key exchange
$ dune exec test/ecdh/server.exe
$ dune exec test/ecdh/client.exe
```

Useful parameters for `server` and `client` to test:

- `--malicious` will send malform point bytes
- `--curve curvename` will change curve point generated

Using these params, point decode will crash, the contract should catch.
