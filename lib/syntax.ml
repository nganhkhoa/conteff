(* Contract syntax, mirroring κ in redex/contracts.rkt:

     κ ::= (flat e) | (κ -> κ) | (κ ->i x κ) | (t κ κ)

   plus box/vector contracts. Contracts are written as OCaml expressions:

     pred                      flat contract, any expression of type 'a -> bool
     c1 @-> c2                 function contract (right associative)
     dep c1 (fun x -> c2)      dependent function contract, x is in scope in c2
     (c1, c2, ...)             tuple contract
     box c                     contract on a Conteff_lib.Box.t
     vector c                  contract on a Conteff_lib.Vector.t

   The older type-payload syntax [@contract : k1 -> k2] is still accepted,
   with identifiers as flat contracts, arrows, tuples, and `k1 -> p dep`
   for (dep k1 (fun x -> p x)). *)

open Ppxlib

type t =
  | Flat of expression
  | Fun of t * t
  | Dep of t * pattern * t
  | Tuple of t list
  | Box of t
  | Vector of t

let rec of_expression (e : expression) : t =
  match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "@->"; _ }; _ },
                [ (Nolabel, dom); (Nolabel, rng) ]) ->
      Fun (of_expression dom, of_expression rng)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "dep"; _ }; _ },
                [ (Nolabel, dom); (Nolabel, rng) ]) -> (
      match rng.pexp_desc with
      | Pexp_function
          ([ { pparam_desc = Pparam_val (Nolabel, None, pat); _ } ], None,
           Pfunction_body body) ->
          Dep (of_expression dom, pat, of_expression body)
      | _ ->
          Location.raise_errorf ~loc:rng.pexp_loc
            "dep expects its range as (fun x -> contract)")
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "box"; _ }; _ },
                [ (Nolabel, c) ]) ->
      Box (of_expression c)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "vector"; _ }; _ },
                [ (Nolabel, c) ]) ->
      Vector (of_expression c)
  | Pexp_tuple cs -> Tuple (List.map of_expression cs)
  | _ -> Flat e

let dep_counter = ref 0

let rec of_core_type (ct : core_type) : t =
  let loc = ct.ptyp_loc in
  match ct.ptyp_desc with
  (* k1 -> p dep: the range predicate also receives the argument, p x r *)
  | Ptyp_arrow (Nolabel, dom, { ptyp_desc = Ptyp_constr ({ txt = Lident "dep"; _ }, [ p ]); _ }) ->
      incr dep_counter;
      let x = Printf.sprintf "__conteff_dep%d" !dep_counter in
      let pred =
        match of_core_type p with
        | Flat e ->
            Ast_builder.Default.(pexp_apply ~loc e [ (Nolabel, pexp_ident ~loc { txt = Lident x; loc }) ])
        | _ -> Location.raise_errorf ~loc:p.ptyp_loc "the range of a dep contract must be a predicate"
      in
      Dep (of_core_type dom, Ast_builder.Default.ppat_var ~loc { txt = x; loc }, Flat pred)
  | Ptyp_arrow (Nolabel, dom, rng) -> Fun (of_core_type dom, of_core_type rng)
  | Ptyp_tuple cs -> Tuple (List.map of_core_type cs)
  | Ptyp_constr ({ txt = Lident "box"; _ }, [ c ]) -> Box (of_core_type c)
  | Ptyp_constr ({ txt = Lident "vector"; _ }, [ c ]) -> Vector (of_core_type c)
  | Ptyp_constr (id, []) -> Flat (Ast_builder.Default.pexp_ident ~loc id)
  | _ ->
      Location.raise_errorf ~loc
        "unsupported contract; write the contract as an expression: [@contract c]"

let of_payload ~loc (p : payload) : t =
  match p with
  | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> of_expression e
  | PTyp ct -> of_core_type ct
  | _ -> Location.raise_errorf ~loc "expected a contract, e.g. [@contract pred @-> pred]"
