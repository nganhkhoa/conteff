# CESK machines

From the repository root, run the proxy-aware mixed typed/untyped CESK machine
with an S-expression program on standard input:

```sh
dune exec examples/cesk-proxy/main.exe < tests/cesk-proxy/team1/1-in.ss
```

The executable parses, validates, links, and evaluates the program, printing
its result or a language error. Field access and mutation have dependent contracts
over the receiver, field,
and resulting value or updated table. Constructor and method arguments and
method results also have conformance contracts. Each machine step checks that
the current and saved environments refer to valid storage locations and that
variables needed by the current transition are bound. The program installs one
contract handler around the pipeline. Contract violations print a
`"protection captured: Machine.Internal.<binding>"` message, using
the generated contract binding label. The machine uses named `let[@contract ...]`
bindings, including a dependent contract on each recursive `step` transition.
Division by zero and unsupported step states currently trigger assertions rather
than language-level errors. Other runtime failures still print
`"run-time error"`. The fixture runner preserves the original expected files
and explicitly maps contract-failure cases to their new diagnostics. Object
shape checks are lazy, and this reference machine only accepts an existing
proxy at another boundary when its shape is equal (it does not compose different
proxy shapes).

The imported assignment 9 input/output fixtures and additional regression tests
are in `tests/cesk-proxy`. Run them from the root with:

```sh
python3 tests/cesk-proxy/run.py
python3 -m unittest discover -s tests/cesk-proxy -p 'test_*.py'
dune runtest  # includes the direct recursive-step contract regression
```

To run selected fixtures, pass a team or test name, for example
`python3 tests/cesk-proxy/run.py team4 team6/3`. The original smaller CESK
example remains available as `dune exec examples/cesk/main.exe`.
