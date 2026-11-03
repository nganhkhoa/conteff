# Contract compilation

The ppx follows the `compile` judgment in `redex/compiler.rkt`. The source construct is
`mon k l j κ e` (`[%mon κ ~pos:k ~neg:l ~loc:j] e`): `k` provides the value, `l` uses it,
and `j` is the contract. A failed check blames `k` with contract location `j`.

## Contracts

| Formal          | Syntax                    |
|-----------------|---------------------------|
| `(flat e)`      | `e` (any `'a -> bool`)    |
| `(κ1 -> κ2)`    | `c1 @-> c2`               |
| `(κ1 ->i x κ2)` | `dep c1 (fun x -> c2)`    |
| `(t κ1 κ2)`     | `(c1, c2)` (n-ary)        |
| boxes, vectors  | `box c`, `vector c`       |

The old type syntax, `[@contract : k1 -> k2]` and `k1 -> p dep`, is still accepted.

## Rules

The contract's predicate expressions are evaluated first, once, and bound to fresh names
(`__conteff_predN`). Predicates in a dependent range are bound under the dependent variable.

**C-Mon-Flat.** The check is only a `perform`, and no handler is installed at the monitor:

```ocaml
Conteff_lib.Contract.check ~pos:k ~loc:j pred e      (* perform 𝒞 (pred, e) *)
```

**C-Mon-Func.** The value is wrapped in lambdas. The argument is monitored with the labels
swapped:

```ocaml
let f = e in fun x -> mon k l j κ2 (f (mon l k j κ1 x))
```

**C-Mon-Dep.** Indy: the range sees the argument monitored with the contract as its
client:

```ocaml
let f = e in fun x -> let y = mon l j j κ1 x in mon k l j κ2 (f (mon l k j κ1 x))
```

**C-Mon-Tuple.** Each component is monitored, from left to right.

**Box / vector.** The ppx builds a view on the same cell. Reads are monitored like results
(`k` is blamed) and writes like arguments (`l` is blamed).

## Handlers

`Check` is handled outside the monitor, so the surrounding context decides what a check
means:

- `Contract.run` is the global handler (`wrap` in the formal model). It runs the predicate
  under a fresh `run`, then resumes with the value or raises `Blame` at the monitor.
- When no handler is installed, `check` falls back to the same behaviour.
- `[%hijack] e` and `[%hijack policy] e` install a handler around `e` (C-Hijack in
  `redex/compiler3.rkt`). `Contract.unchecked` skips checks. `enforce`, `warn`,
  `observe`, `reassign` and `delegate` are other policies. A user-written
  `{ on_check = ... }` works too.

A hijack covers the *dynamic extent* of `e`. A function contract created inside
`[%hijack]` but called outside it is still checked.

## Let-bound contracts

```ocaml
let[@contract c] x = e
```

becomes

```ocaml
let __raw_x = e
let __mon_x neg = mon "M.x" neg "M.x" c __raw_x
let x = __mon_x "external"        (* only at the top level of a module *)
```

Every unqualified use of `x` in the same compilation unit is replaced by
`__mon_x "<enclosing definition>"`. That makes the client label the definition containing
the use. Shadowing by `let`, function parameters, `match` cases and `for` is respected. For
`let rec`, recursive calls are monitored too, and they blame the function itself as the
client. Qualified uses from other modules go through the exported `x`, so they blame
`"external"`.

A top-level flat contract is checked when the module is initialised.
