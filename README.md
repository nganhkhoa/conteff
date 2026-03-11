# CONtract To EFFects

This project tries to implement software contracts using effects and effect handlers. OCaml has support for effects, but not contracts, which is an ideal choice for implementation.

## Use

Add this project to the dune dependency.

```
opam pin add conteff git+https://github.com/yourusername/your-repo-name.git
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

Annotate the desire function/variables with contracts.

```ocaml
(* bigger_0 and bigger_10 is defined correctly else compile error *)

let [@contract : bigger_0] x = 1

let [@contract : bigger_0 -> bigger_10] f x = x + 10
```

## Build and test

```sh
$ dune build
$ dune describe pp test/manual/somefile.ml
```
