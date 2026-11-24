(* Contract syntax, mirroring κ in redex/contracts.rkt:

     κ ::= (flat e) | (κ -> κ) | (κ ->i x κ) | (t κ κ)

   plus box/vector contracts. Contracts are written as OCaml expressions:

     pred                      flat contract, any expression of type 'a -> bool
     custom projection         custom projection, of type
                                pos:_ -> neg:_ -> loc:_ -> 'a -> 'a
     c1 @-> c2                 function contract (right associative)
     dep c1 (fun x -> c2)      dependent function contract, x is in scope in c2
     dep_args [(x, c1); (y, c2)] (fun x y -> c3)
                                curried dependent contract, equivalent to
                                dep c1 (fun x -> dep c2 (fun y -> c3))
                                Each list binder is a distinct identifier;
                                earlier binders scope over later contracts,
                                and the result function must name all binders
                                in the same order. The list must be nonempty.
                                For example:
     dep_args [(x, is_pos); (y, gt x)] (fun x y -> gt (x + y))
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

(* Keep projections inside a recognizable expression marker so existing
   syntax traversals visit their contents just like flat predicates.  The
   marker is consumed by code generation and never reaches emitted OCaml. *)
let custom_marker = "__conteff_custom_projection"

let custom ~loc projection =
  Flat (Ast_builder.Default.pexp_extension ~loc
    ({ txt = custom_marker; loc }, PStr [ Ast_builder.Default.pstr_eval ~loc projection [] ]))

let rec of_expression (e : expression) : t =
  match e.pexp_desc with
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "custom"; _ }; _ },
                [ (Nolabel, projection) ]) ->
      custom ~loc:e.pexp_loc projection
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "custom"; _ }; _ }, _) ->
      Location.raise_errorf ~loc:e.pexp_loc
        "custom expects a single projection expression"
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "@->"; _ }; _ },
                [ (Nolabel, dom); (Nolabel, rng) ]) ->
      Fun (of_expression dom, of_expression rng)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "dep_args"; _ }; _ },
                [ (Nolabel, args); (Nolabel, result) ]) ->
      let rec entries e =
        match e.pexp_desc with
        | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> []
        | Pexp_construct ({ txt = Lident "::"; _ },
                          Some { pexp_desc = Pexp_tuple [entry; tail]; _ }) ->
            let name, contract =
              match entry.pexp_desc with
              | Pexp_tuple
                  [ { pexp_desc = Pexp_ident { txt = Lident name; loc }; _ }; contract ] ->
                  ({ txt = name; loc }, contract)
              | _ ->
                  Location.raise_errorf ~loc:entry.pexp_loc
                    "dep_args expects entries of the form (identifier, contract)"
            in
            (name, contract) :: entries tail
        | _ ->
            Location.raise_errorf ~loc:e.pexp_loc
              "dep_args expects a literal list of (identifier, contract) entries"
      in
      let args = entries args in
      if args = [] then
        Location.raise_errorf ~loc:e.pexp_loc "dep_args requires at least one argument";
      let names = List.map (fun (name, _) -> name.txt) args in
      if List.length (List.sort_uniq String.compare names) <> List.length names then
        Location.raise_errorf ~loc:e.pexp_loc "dep_args requires distinct argument names";
      let body =
        match result.pexp_desc with
        | Pexp_function (params, None, Pfunction_body body)
          when List.length params = List.length names &&
               List.for_all2 (fun param name ->
                 match param.pparam_desc with
                 | Pparam_val (Nolabel, None, { ppat_desc = Ppat_var var; _ }) ->
                     String.equal var.txt name
                 | _ -> false) params names -> body
        | _ ->
            Location.raise_errorf ~loc:result.pexp_loc
              "dep_args expects (fun x y ... -> contract) with the listed names in order"
      in
      List.fold_right
        (fun (name, dom) rng ->
          Dep (of_expression dom, Ast_builder.Default.ppat_var ~loc:name.loc name, rng))
        args (of_expression body)
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "dep_args"; _ }; _ }, _) ->
      Location.raise_errorf ~loc:e.pexp_loc
        "dep_args expects a list and a result function"
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

(* [@effect_contract Op (request |> response)] selects one unary operation.
   The pipe is syntax only; neither predicate is applied to the other. *)
type effect_contract = {
  operation : Longident.t loc;
  request : expression;
  response : expression;
}

let effect_of_payload ~loc = function
  | PStr [{ pstr_desc = Pstr_eval
      ({ pexp_desc = Pexp_construct (operation,
          Some { pexp_desc = Pexp_apply
            ({ pexp_desc = Pexp_ident { txt = Lident "|>"; _ }; _ },
             [Nolabel, request; Nolabel, response]); _ }); _ }, _); _ }] ->
      { operation; request; response }
  | _ ->
      Location.raise_errorf ~loc
        "expected [@effect_contract Operation (request_predicate |> response_predicate)]"
