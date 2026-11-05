# Macros, expansion, and blame

Conteff's ppx rewrites OCaml surface syntax into ordinary OCaml plus the
`Conteff_lib` runtime. It runs the named-state pass before the contract pass.
The macros are compile-time syntax, not ordinary functions. Enable the ppx for
the relevant compilation unit as described in [running.md](running.md). For
the underlying contract rules, see [contract-rewrite.md](contract-rewrite.md).

To inspect the actual output (generated names and source labels depend on the
file and the rest of its contents):

```sh
dune exec tests/expand/pp.exe -- --impl path/to/file.ml
```

## Surface forms

| Form | Meaning |
|---|---|
| `[%mon c ~pos:p ~neg:n ~loc:j] e` | Monitor `e` with contract `c`; all three labels are optional. |
| `let[@contract c] x = e` | Bind a contracted value; supported at structure and expression level, including `rec`. |
| `[%hijack] e`, `[%hijack policy] e` | Handle checks in `e` with `unchecked` or the given policy. |
| `[%contract_main] e` | Run `e` under the default check handler. |
| `let%contract_main () = e` | Structure-level unit binding whose RHS runs under the default handler. |
| `[%contract_main_state initial] e` | Run `e` with one integer state and check handler, returning only its result. |
| `[%%contract_states let a : t = init and ...]` | Declare a group of typed named states and generate its operations and handler. |
| `[%contract_get_a]`, `[%contract_set_a e]` | Get or set the declared named state `a`. |
| `[%contract_main_states] e` | Run `e` with the generated handler and fresh initial values, returning only its result. |

### Contract language

* `pred` is a flat contract: an expression of type `'a -> bool`.
* `c1 @-> c2` is a function contract; arrows associate to the right.
* `dep c1 (fun x -> c2)` is a dependent function contract. Its range can
  refer to `x`; the range must be syntactically a single-parameter `fun`.
* `(c1, c2, ...)` monitors a tuple component by component, left to right.
* `box c` and `vector c` monitor `Conteff_lib.Box.t` and
  `Conteff_lib.Vector.t`, respectively, not arbitrary OCaml refs or arrays.

The older type-payload binding syntax remains supported, e.g.
`let[@contract : is_pos -> is_pos] f x = x` and
`let[@contract : is_pos -> above dep] f x = x`, where `above x result`
produces the dependent range predicate. Expression payloads are more general.

Predicate expressions are hoisted and evaluated once per monitor construction,
left to right, before the monitored expression. Dependent range predicates are
instead constructed under the dependent binder on each call. Function bodies,
box operations, and vector operations are checked when used, not merely when
the wrapper is constructed.

## Monitor expansions

The following are actual `pp` output excerpts from one file named
`macro_examples.ml`; fresh numbers are incidental. For example:

```ocaml
let a = [%mon pred ~pos:"k" ~neg:"l" ~loc:"j"] value
```

expands to:

```ocaml
let a =
  let __conteff_pred1 = pred in
  Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred1 value
```

`check` performs the `Check` effect; the monitor does **not** install a
handler. Under the default handler a false predicate raises `Contract.Blame`
at the monitor, and a predicate exception is propagated there. Without a handler
the effect is unhandled (`Effect.Unhandled`).

For `let f = [%mon (yes @-> no) ~pos:"k" ~neg:"l" ~loc:"j"] g`,
`pp` emits:

```ocaml
let f =
  let __conteff_pred2 = yes in
  let __conteff_pred3 = no in
  let __conteff_f4 = g in
  fun __conteff_x5 ->
    Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred3
      (__conteff_f4
         (Conteff_lib.Contract.check ~pos:"l" ~loc:"j" __conteff_pred2
            __conteff_x5))
```

For `let d = [%mon dep yes (fun y -> gt y) ~pos:"k" ~neg:"l" ~loc:"j"] g`:

```ocaml
let d =
  let __conteff_pred6 = yes in
  let __conteff_f7 = g in
  fun __conteff_x8 ->
    let y =
      Conteff_lib.Contract.check ~pos:"l" ~loc:"j" __conteff_pred6
        __conteff_x8 in
    let __conteff_pred9 = gt y in
    Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred9
      (__conteff_f7
         (Conteff_lib.Contract.check ~pos:"l" ~loc:"j" __conteff_pred6
            __conteff_x8))
```

The domain is monitored twice: the range's dependent argument uses labels
`(l, j, j)` and the argument passed to `g` uses `(l, k, j)`. This matters for
higher-order domains; for effectful flat predicates the two checks also mean
the predicate can run twice per function call.

For `let p = [%mon (yes, no) ~pos:"k" ~neg:"l" ~loc:"j"] (1, 2)`:

```ocaml
let p =
  let __conteff_pred10 = yes in
  let __conteff_pred11 = no in
  let (__conteff_v12, __conteff_v13) = (1, 2) in
  let __conteff_r14 =
    Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred10
      __conteff_v12 in
  let __conteff_r15 =
    Conteff_lib.Contract.check ~pos:"k" ~loc:"j" __conteff_pred11
      __conteff_v13 in
  (__conteff_r14, __conteff_r15)
```

For a box or vector, expansion constructs a view over the original object:
reads monitor with `(pos, neg, loc)` and writes monitor with
`(neg, pos, loc)` before updating the underlying object. A vector view also
captures the underlying length. Nested contracts apply recursively to values
read or written; the view is not a copy.

## Blame labels

A default-handler failure raises
`Conteff_lib.Contract.Blame { party; contract }`. The labels are strings;
`party` identifies the responsible party and `contract` identifies the
contract. Explicit monitor labels can themselves be expressions. In
`[%mon c ~pos:k ~neg:l ~loc:j] e`, `k` is the provider, `l` the client, and
`j` the contract. Defaults are `"server"`, `"client"`, and the source
basename plus line (e.g. `"main_state.ml:1"`), respectively. Defaults do not
infer an enclosing definition for an explicit `[%mon]`.

| Failure in a monitor with `(k, l, j)` | Default-handler blame |
|---|---|
| Flat predicate, tuple component, or box/vector read | `party = k; contract = j` |
| Function argument or box/vector write | `party = l; contract = j` |
| Function result | `party = k; contract = j` |
| Higher-order argument's result, supplied by the client | `party = l; contract = j` |
| Bad argument supplied by the server to a higher-order argument | `party = k; contract = j` |
| Dependent range misuses its higher-order dependent argument | `party = j; contract = j` (indy blame) |

These rules recurse: each function-domain or write boundary swaps positive and
negative labels, while `loc` stays fixed. In particular, the dependent case
monitors the range's view of the argument with `j` as its client; it does not
assign all dependent-contract failures to `j`. A predicate raising its own
exception propagates that exception rather than manufacturing blame. Custom
policies can skip checks, report instead of raising, or change the party.

### Labels for contracted bindings

For a binding `x` in module path `M`, the generated monitor has positive and
contract labels `"M.x"`. Each unqualified use in the same compilation unit
passes the path of the enclosing definition as negative label. For example,
`tests/unit/test_let.ml` checks:

| Scenario | `party` | `contract` |
|---|---|---|
| `f` returns a bad result | `Test_let.f` | `Test_let.f` |
| `test_client` calls `f` with a bad argument | `Test_let.test_client` | `Test_let.f` |
| `Server.local_use` calls its local `g` badly | `Test_let.Server.local_use` | `Test_let.Server.g` |
| A qualified `Server.g` use fails its domain | `external` | `Test_let.Server.g` |
| Local flat `n` fails in `local_flat_fail` | `Test_let.local_flat_fail.n` | `Test_let.local_flat_fail.n` |

The root is the capitalized source-file basename without extension; nested
module and definition names extend the path. A first-class unqualified use also
gets the enclosing definition's label. Recursive unqualified calls are
monitored and use the recursive function's own label as client. Qualified uses
go through the exported binding, whose client is `"external"`, even when the
qualified use appears in the same compilation unit. Shadowing by let bindings,
function parameters, match patterns and for-loop variables is respected.

For example, in `macro_examples.ml`, the following:

```ocaml
let[@contract pos @-> pos] id x = x
let client () = id 1
```

produces the following `pp` output (the function export is eta-expanded):

```ocaml
let __conteff_raw_id_17 x = x
let __conteff_mon_id_16 __conteff_neg18 =
  let __conteff_pred19 = pos in
  let __conteff_pred20 = pos in
  let __conteff_f21 = __conteff_raw_id_17 in
  fun __conteff_x22 ->
    Conteff_lib.Contract.check ~pos:"Macro_examples.id"
      ~loc:"Macro_examples.id" __conteff_pred20
      (__conteff_f21
         (Conteff_lib.Contract.check ~pos:__conteff_neg18
            ~loc:"Macro_examples.id" __conteff_pred19 __conteff_x22))
let id __conteff_a23 = __conteff_mon_id_16 "external" __conteff_a23[@@warning
                                                                    "-32"]
let client () = (__conteff_mon_id_16 "Macro_examples.client") 1
```

A top-level flat contracted binding is checked while its exported binding is
initialized, so a suitable handler must cover that initialization. Local
contracted bindings generate raw and monitor bindings in their lexical scope;
there is no exported `external` binding for them.

## Handlers and entry points

From `pp`, `let h = [%hijack] work`, `let m = [%contract_main] work`, and
`let s = [%contract_main_state 0] work` become:

```ocaml
let h =
  Conteff_lib.Contract.with_policy Conteff_lib.Contract.unchecked
    (fun () -> work)
let m = Conteff_lib.Contract.run (fun () -> work)
let s = Conteff_lib.Contract.with_state ~initial:0 (fun () -> work)
```

`let%contract_main () = e` similarly wraps the RHS in `Contract.run`; it
requires one nonrecursive unit binding. `[%contract_main]` takes no payload.
The handler covers the dynamic execution of its body, not later calls to a
function returned by it. The same is true for hijacking: a wrapper created
inside a hijack but called afterwards has its later checks handled outside it.

`[%hijack policy] e` uses `Contract.with_policy policy (fun () -> e)`.
Built-in policies include `unchecked` (skip predicate and return the value),
`enforce`, `delegate` (forward to an outer handler), `warn report`,
`observe callback next`, and `reassign party next`. A custom `Contract.policy`
provides a polymorphic `on_check`; its returned value resumes the check and its
exception is raised at the monitor. Nested handlers determine which policy sees
each check.

## State-aware contracts

### One integer state

`[%contract_main_state initial] e` evaluates the initial integer expression
and runs `e` with `Contract.with_state`. Use `Contract.get_state ()` and
`Contract.set_state n` in the body or predicates. The macro returns the body
result and discards final state; use the runtime
`Contract.with_state_result ~initial (fun () -> e)` or
`Contract.run_state (fun () -> e) initial` when the final `(result, state)`
pair is needed. For example:

```ocaml
open Conteff_lib

let increasing x =
  let previous = Contract.get_state () in
  if x < previous then false
  else (Contract.set_state x; true)

let[@contract increasing @-> Contract.any] double x = x * 2

let () = [%contract_main_state 0] (
  let values = List.map double [1; 2; 4] in
  Printf.printf "last: %d\n" (Contract.get_state ());
  ignore values)
```

Here the state is initialized to `0`, so negative first inputs are rejected.
The handler covers the calls to `double`, not just creation of its wrapper.
`examples/state/increment.ml` provides another runnable example:

```sh
dune exec examples/state/increment.exe
```

These operations require a state handler.

### Multiple named, heterogeneous states

```ocaml
[%%contract_states let a : int = 0 and b : string = ""]
let result = [%contract_main_states]
  (let x = [%contract_get_a] in
   [%contract_set_a (x + 1)];
   [%contract_get_b])
```

The declaration generates typed `Get`/`Set` effects and helper functions for
each name plus one shared shallow handler. Each entry-point execution evaluates
each initializer once, in declaration order, and starts a fresh state. The
handler internally threads a heterogeneous nested tuple and discards final
state on normal return. The `pp` output for the result is:

```ocaml
let result =
  __conteff_contract_states_run
    (fun () ->
       let x = __conteff_contract_state_a_get () in
       __conteff_contract_state_a_set (x + 1);
       __conteff_contract_state_b_get ())
```

For a concrete expanded program with `int` and `string` slots, see
[`multiple.expanded.ml`](../macro-expansion/expanded/examples/state/multiple.expanded.ml).
The generated code includes these pieces (formatting simplified):

```ocaml
type _ Effect.t +=
  | Conteff_contract_state_calls_Get : int Effect.t
  | Conteff_contract_state_calls_Set : int -> unit Effect.t

type _ Effect.t +=
  | Conteff_contract_state_last_argument_Get : string Effect.t
  | Conteff_contract_state_last_argument_Set : string -> unit Effect.t

let __conteff_contract_state_calls_get () =
  Effect.perform Conteff_contract_state_calls_Get
let __conteff_contract_state_calls_set value =
  Effect.perform (Conteff_contract_state_calls_Set value)
```

Its runner initializes both states and uses an answer type containing
`int * (string * unit)`. Its `effc` has one branch for each get and set plus a
`Contract.Check` branch. That check branch installs the same handler around
the predicate and resumed continuation, so their state changes are threaded
together. Inspect the complete output or run the source with:

```sh
dune exec tests/expand/pp.exe -- --impl examples/state/multiple.ml
dune exec examples/state/multiple.exe
```

For a contract that rejects inputs below the previous accepted input, run
`dune exec examples/state/increasing.exe`; its full expansion is
[`increasing.expanded.ml`](../macro-expansion/expanded/examples/state/increasing.expanded.ml).
Its initial state is `None`, so the first integer is accepted regardless of sign.

Use the declaration before references to its generated operations and entry
point, in their lexical scope. Names must be unique within the declaration;
get has no payload, set takes one expression. The generated runner has a fixed
name, so avoid multiple declaration groups in the same scope. Named operations
without their corresponding generated handler are unhandled effects if no
appropriate outer handler exists.

Both state handlers also handle `Check` themselves: predicates, nested checks,
and resumed continuation share the state. Predicate updates survive a failed
check or predicate exception when that exception is caught inside the handled
body, and subsequent state operations still work. See
`tests/unit/test_state.ml` and `tests/unit/test_multi_state.ml`. Do not put an
ordinary `contract_main` handler around the part whose checks need to be
handled by the state-aware handler: the nearest handler handles the check.

## Diagnostics and verification

The ppx rejects an unapplied `[%mon]`, `[%hijack]`, or entry-point extension;
`dep` without a syntactic `fun` range; and `[@contract]` on a non-variable
pattern. State declarations require a nonempty nonrecursive group of typed,
uniquely named bindings. Missing state names, unexpected get payloads, missing set
expressions, and malformed entry-point payloads are errors. See
`tests/expand/errors.t/run.t` for exact diagnostics and
`tests/expand/compile.t/run.t` for reproducible expansion snapshots. Runtime
blame and higher-order cases are exercised in `tests/unit/test_flat.ml`,
`test_function.ml`, `test_dependent.ml`, `test_tuple.ml`, `test_box.ml`, and
`test_let.ml`.
