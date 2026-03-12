# Contract rewriting

## Annotations

```ocaml
(* flat contract *)
let [@contract : predicate] x = value
(* higher order contract *)
let [@contract : predicate -> predicate] f = body
(* dependent contract *)
let [@contract : predicate -> predicate deps] f = body
(* trace contract *)
let [@contract : predicate -> constructor predicate trace] f = body
```

## Rewriting

Given a `let` binding annotated with `[@contract]`, the preprocessor will rewrite accordingly to the rules below. The contract provided must conform to the types, and we need the types for all variables, parameters used. All values are also annotated now with the labels `pos, neg, cloc` representing positive, negative and contract location respectively. At callsite, these will be annotated correctly. This construction lets the recursive behavior uses the correct blame labels.

```ocaml
(* original *)
let [@contract : Kontract] var : v_type = body

(* rewritten *)
let var pos neg cloc =
  let module Contract = struct
    type _ Effect.t += v : v_type -> v_type Effect.t
  end in

  let handler =
    {
      effc = fun (type e) (eff : e Effect.t) ->
        match eff with
        | Contract.v v -> Some (fun k ->
            let valid = Kontract v in
            if valid
            then continue k ()
            else discontinue k (Blame pos)
          )
        | _ -> None
    }
  in

  try_with (fun () ->
    Effect.perform (Contract.v body);
  ) ()
  handler
```

```ocaml
let
  [@contract : contract_for_x -> contract_for_y -> contract_for_z -> contract_for_f]
  f x y z = body

(* compiles into *)

let f x y z =
  let [@contract : contract_for_x] x = x in
  let [@contract : contract_for_y] y = y in
  let [@contract : contract_for_z] z = z in

  (* define a new return value *)
  let [@contract : contract_for_f] ret = body in
  ret
```

## Blame Labels handling

We provide 3 blame labels `pos, neg, cloc` which denotes the same idea as the formal monitor construct $mon^{pos,neg}_{cloc}$. These will be added by rewriting the let expression to a function receiving extra 3 arguments, `pos, neg, cloc`. Placing the correct labels is essential to the implementation, below we analyze some cases:

1. The variable is a global scope

```ocaml
module A = struct
let [@contract : ...] x = value

let use = (x "A" "A.use" "A.x")
end
```

`pos` is the owner module, `neg` is the callsite (we are still deciding on the callsite module or callsite function), `cloc` is the combination of owner module and the variable.

2. The variable comes from function argument

Because we rewrite the arguments into contracts, we follow the labels rule and switch the `pos,neg` of the function for its arguments.

```ocaml
module A = struct
(* k1 -> k2 *)
let f pos neg cloc x =
  (* arguments are flipped *)
  let x pos neg cloc = contract_rewritten x in
  let x = (x neg pos cloc) in

  (* return value is intact *)
  let __ret__ pos neg cloc = contract_rewritten body in
  let __ret__ = (__ret__ pos neg cloc) in
  __ret__
end
```

Dependent contract needs the arguments with different labels:

```ocaml
module A = struct
(* k1 -> k2 dep *)
let f pos neg cloc x =
  (* arguments are flipped *)
  let x pos neg cloc = contract_rewritten x in

  (* run the contract k1 with different labels, and provide to k2 *)
  let __ret__contract__ = k2 (x neg cloc cloc) in
  (* redefine argument with differnt labels *)
  let x = (x neg pos cloc) in

  (* ^ be careful when shadowing *)

  let __ret__ pos neg cloc = contract_rewritten body in
  let __ret__ = (__ret__ pos neg cloc) in
  __ret__
end
```

3. The variable is defined inside a function/value

Because we don't restrict where the user can or cannot place contract annotations, they can write programs with contracts in some nested place.

```ocaml
module A = struct
let [@contract : k1] x =
  let [@contract : k2] y = value in
  let f z = z + y in
  f 0
end
```

Defining blame labels for this can be troublesome, here is one trivial way. However, this needs precise tracking of scope to correctly assign the blame during rewrite.

```ocaml
module A = struct
let [@contract : k1] x =
  let y pos neg cloc = value in
  let f z = z + (y "A.x" "A.x.f" "A.x.y") in
  f 0
end
```
