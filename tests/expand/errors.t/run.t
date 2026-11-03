Malformed uses are reported at compile time.

  $ cat > a.ml <<'X'
  > let[@contract pos] (a, b) = (1, 2)
  > X
  $ ../pp.exe --impl a.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [@contract] can only annotate a binding of a single variable

  $ cat > b.ml <<'X'
  > let x = [%mon pos]
  > X
  $ ../pp.exe --impl b.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%mon c] must be applied to an expression: [%mon c] e

  $ cat > c.ml <<'X'
  > let f = [%mon dep pos pos] g
  > X
  $ ../pp.exe --impl c.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: dep expects its range as (fun x -> contract)

  $ cat > d.ml <<'X'
  > let x = [%hijack]
  > X
  $ ../pp.exe --impl d.ml 2>&1 | grep -v '^\s*$' | tail -1
  Error: [%hijack] must be applied to an expression: [%hijack] e
