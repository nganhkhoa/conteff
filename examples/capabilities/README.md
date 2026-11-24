# Contract capability examples

Executable OCaml counterparts to the selected capability illustrations in
`rewrite/redex/capabilities`, inspired by Table 1 of Moy, Dimoulas, and
Felleisen, *Effectful Software Contracts*. These are small examples built on
Conteff's contract macros and attributes, not a full implementation of the paper or a proof
of equivalence to the Redex models. Each includes passing and failing cases
and assertions about blame or state. Dune enables the Conteff PPX for these
examples. OCaml code remains necessary for predicates, effect operations and
handlers, and test assertions; the macros generate the contract machinery.

From the `impl` repository root (OCaml 5 and Dune 3.20 or later):

```sh
dune exec examples/capabilities/allow_call.exe
dune exec examples/capabilities/restricted_effect.exe
dune exec examples/capabilities/union_contracts_v2.exe
# Or run all examples as tests:
dune runtest examples/capabilities
```

| Capability | Example | What is demonstrated |
|---|---|---|
| Allow call | `allow_call.ml` | A ghost permission bit gates callback entry. |
| Ghost state | `ghost_state.ml` | Ghost state enforces A-before-B, then a final contract checks completion and resets it. |
| Must call | `must_call.ml` | A callback records entry; a final check requires it. |
| Non-reentrant | `non_reentrant.ml` | Entry and normal-return checks manage a busy bit. |
| Termination | `termination.ml` | Instrumented finite entries must have decreasing natural ranks. |
| Exceptions | `exceptions.ml` | An explicit algebraic raise request is rejected or handled by an outer aborting handler. |
| Framing | `framing.ml` | A boundary permits reads and restricts writes to selected box IDs in an effect-handled store. |
| Purity | `purity.ml` | A boundary rejects selected escaping algebraic operations. |
| Restricted effect | `restricted_effect.ml` | A boundary checks requests and responses. |
| Union contracts | `union_contracts.ml` | History-sensitive higher-order unions with polarity-sensitive blame, installed with the custom projection syntax. |
| Union contracts, manual | `union_contracts_v2.ml` | The same union projections with explicit tree nodes and a handwritten state and check handler, without PPX syntax. |

State examples use a trusted shared state handler and trusted instrumentation,
not private per-instance state. The ghost-state protocol resets on normal return,
but its final contract cannot clean up after exceptions or divergence. In particular, allow-call and non-reentrant
cleanup only on normal return; must-call checks only at its normal-return
checkpoint; termination cannot detect arbitrary divergence. Programs that can
forge state or bypass instrumentation can defeat these examples.

Effect boundaries select explicit OCaml effect constructors. Unselected
operations, operations handled inside the boundary, native mutation, and host
exceptions are not intercepted. Framing covers only its explicit box effects and does not prove heap separation, and
purity does not cover arbitrary OCaml effects. An aborting handler does not
produce a response to check. Unlike the experimental Redex effect boundary,
the OCaml runtime blames the surrounding handler for an invalid response, and
the provider for an invalid request. Union projections allocate distinct entries
in handler-managed state; the handler must encompass projection and all uses of
the monitored values. Effects of failed alternatives are not rolled back.
See the source comments and tests for further caveats.
