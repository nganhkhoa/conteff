# Contract compilation

The ppx compiles monitors into ordinary OCaml and the `Conteff_lib` runtime.
The core rules follow the `compile` judgment referenced in `lib/codegen.ml`;
the OCaml implementation also supports mutable views and state-aware handlers.
For the complete surface syntax, examples, and actual expansion excerpts, see
[macros.md](macros.md).

Write a monitor as `mon k l j κ e`, or
`[%mon κ ~pos:k ~neg:l ~loc:j] e`: `k` provides the value, `l` uses it,
and `j` identifies the contract. With the default handler, a failed flat
check blames the current positive label and records `j` as the contract.
The labels on an explicit monitor are optional: they default to `"server"`,
`"client"`, and the source basename and line, respectively.

## Contracts

| Formal / kind | Expression syntax |
|---|---|
| `(flat e)` | `e` (an expression of type `'a -> bool`) |
| `(κ1 -> κ2)` | `c1 @-> c2` (right associative) |
| `(κ1 ->i x κ2)` | `dep c1 (fun x -> c2)` |
| Curried dependent contracts | `dep_args [(x, c1); (y, c2)] (fun x y -> c3)` |
| `(t κ1 κ2 ...)` | `(c1, c2, ...)` |
| Box / vector | `box c`, `vector c` |

`dep_args [(x, c1); (y, c2)] (fun x y -> c3)` is parsed as
`dep c1 (fun x -> dep c2 (fun y -> c3))`, not as a contract on a tuple.
Its list must be a nonempty literal list with distinct identifier names, and
its result function must have exactly those unlabelled parameters in order.
Each binder scopes over later domains and the result, but not its own domain.
Thus partial application and higher-order blame follow the nested `dep` rules.
Both `dep_args` and `dep` work in explicit monitors and binding contracts.
The older binding type-payload syntax, `[@contract : k1 -> k2]` and
`[@contract : k1 -> p dep]`, is still accepted.

## Rules

For each monitor construction, predicate expressions outside dependent ranges
are hoisted into fresh bindings (`__conteff_predN`), left to right, before
the monitored expression. A dependent range is hoisted under its binder at
each invocation, so later stages of a `dep_args` contract are constructed at
the corresponding partial application. Predicate *execution* occurs at each
check, not at hoisting. The following rules are schematic; labels and names
in generated code may differ.

**C-Mon-Flat.** The check performs `Check`; the monitor installs no handler:

```ocaml
Conteff_lib.Contract.check ~pos:k ~loc:j pred e
```

**C-Mon-Func.** The value is wrapped in a lambda. The argument is monitored
with labels swapped, and the result with the original labels:

```ocaml
let f = e in fun x -> mon k l j κ2 (f (mon l k j κ1 x))
```

**C-Mon-Dep.** Indy: the range sees the argument monitored with the contract
as its client:

```ocaml
let f = e in fun x ->
  let y = mon l j j κ1 x in
  mon k l j κ2 (f (mon l k j κ1 x))
```

The range's predicates are constructed after binding `y`, before the second
monitor and call to `f`. The domain is monitored twice; an effectful flat
predicate can therefore run twice per call. For a higher-order domain, a
range contract that misuses its view of the argument can blame `j`; the
argument supplied to `f` instead uses `k` as its client.

**C-Mon-Tuple.** The tuple expression is evaluated once and its components
are monitored from left to right.

**Box / vector.** The ppx builds a view on the same `Conteff_lib.Box.t` or
`Conteff_lib.Vector.t`, not an OCaml ref or array. Reads are monitored like
results (`k` is positive) and writes like arguments (`l` is positive), before
updating the underlying object. The vector view captures the underlying
length. Nested contracts apply recursively.

## Handlers and state

`Check` is handled outside the monitor, so the surrounding dynamic context
decides what a check means:

- `Contract.run` and `[%contract_main] e` install the default handler. It
  handles the predicate and continuation, resuming with the value on success,
  or raising `Contract.Blame { party; contract }` at the monitor on failure.
  Predicate exceptions propagate at the monitor. With no handler, `check`
  raises `Effect.Unhandled`.
- `[%hijack] e` and `[%hijack policy] e` install a policy handler around `e`.
  The default `Contract.unchecked` skips checks; `enforce`, `warn`, `observe`,
  `reassign`, `delegate`, and custom `{ on_check = ... }` policies provide
  other behavior. See [macros.md](macros.md#handlers-and-entry-points) for
  their forms.
- `[%contract_main_state initial] e` uses `Contract.with_state` for a single
  integer state. `[%%contract_states let a : t = init and ...]` generates typed
  named get/set effects and a shared handler; `[%contract_main_states] e`
  invokes its runner with fresh initial values. Both state handlers handle
  `Check` as well as their state operations: predicate, nested checks, and
  resumed continuation share the threaded state. The macros return the body
  result, discarding final state. For operations, initialization, and runtime
  alternatives, see [state-aware contracts](macros.md#state-aware-contracts).

Handlers cover the *dynamic extent* of their bodies. In particular, a function
wrapper created inside `[%hijack]` but called outside it performs its later
checks outside that hijack; those checks need an appropriate surrounding
handler. The nearest handler for `Check` determines its behavior.

## Let-bound contracts

Schematically, a nonrecursive structure binding

```ocaml
let[@contract c] x = e
```

becomes

```ocaml
let __raw_x = e
let __mon_x neg = mon "M.x" neg "M.x" c __raw_x
let x = __mon_x "external"       (* non-function contract export *)
```

For a function contract the export is eta-expanded instead:
`let x a = __mon_x "external" a`. Local contracted bindings have raw and
monitor bindings in their lexical scope but no exported `external` binding.
Each invocation of the generated monitor constructs its own monitor and
hoists its predicates; in particular, the function export constructs one on
application, rather than at module initialization.

Unqualified uses of `x` in the same compilation unit are replaced by
`__mon_x "<enclosing definition>"`, making the client label the definition
containing the use. Shadowing by `let`, function parameters, `match` cases
and `for` is respected. For `let rec`, recursive calls are monitored too,
and they blame the function itself as client. Qualified uses, including
those in the same compilation unit, go through the exported `x` and use
`"external"` as client. A top-level flat contract is checked when its export
is initialized, so a handler must cover that initialization.
